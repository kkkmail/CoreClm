namespace WorkerNodeService

open System
open Argu
open ClmSys.GeneralData
open ClmSys
open ClmSys.Logging
open ClmSys.WorkerNodeData
open ClmSys.TimerEvents
open ContGenServiceInfo.ServiceInfo
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.SvcCommandLine
open ServiceProxy.MsgServiceProxy
open MessagingServiceInfo.ServiceInfo
open Messaging.Client
open Messaging.ServiceResponse
open Clm.ModelParams
open ServiceProxy.WorkerNodeProxy
open ServiceProxy.MsgProcessorProxy
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.WorkerNodeErrors
open ClmSys.WorkerNodePrimitives
open ClmSys.MessagingPrimitives
open ServiceProxy.SolverRunner
open ClmSys.Rop
open SolverRunner.SolverRunnerTasks
open ClmSys.SolverRunnerPrimitives
open ClmSys.MessagingData

module ServiceImplementation =

    let private toError g f = f |> g |> WorkerNodeErr |> Error
    let private addError g f e = ((f |> g |> WorkerNodeErr) + e) |> Error


    let mutable serviceAccessInfo =
        let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = workerNodeServiceProgramName)
        let results = (parser.Parse [||]).GetAllResults()
        results |> getServiceAccessInfo


    type WorkerNodeMessageResult = MessageProcessorResult<WorkerNodeRunnerState * UnitResult>


    type WorkerNodeRunnerState
        with

        static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]

        static member defaultValue =
            {
                runningWorkers = Map.empty
                numberOfWorkerCores = 0
                workerNodeState = NotStartedWorkerNode
            }


    type WorkerNodeRunnerData =
        {
            workerNodeServiceInfo : WorkerNodeServiceInfo
            workerNodeProxy : WorkerNodeProxy
            messageProcessorProxy : MessageProcessorProxy
            minUsefulEe : MinUsefulEe
        }


    type SendMessageProxy =
        {
            partitionerId : PartitionerId
            sendMessage : MessageInfo -> UnitResult
        }


    type OnRegisterProxy =
        {
            workerNodeInfo : WorkerNodeInfo
            sendMessageProxy : SendMessageProxy
        }


    type OnUpdateProgressProxy =
        {
            tryDeleteWorkerNodeRunModelData : RunQueueId -> UnitResult
            sendMessageProxy : SendMessageProxy
        }


    let onSaveResult (proxy : SendMessageProxy) (r : ResultDataWithId) =
        printfn "onSaveResult: Sending results with resultDataId = %A." r.resultDataId

        {
            partitionerRecipient = proxy.partitionerId
            deliveryType = GuaranteedDelivery
            messageData = r |> SaveResultPrtMsg
        }.getMessageInfo()
        |> proxy.sendMessage
        |> bindError (addError OnSaveResultErr (SendResultMessageError (proxy.partitionerId.messagingClientId, r.resultDataId)))


    let onSaveCharts (proxy : SendMessageProxy) (r : ChartGenerationResult) =
        match r with
        | GeneratedCharts c ->
            printfn "onSaveCharts: Sending charts with resultDataId = %A." c.resultDataId

            {
                partitionerRecipient = proxy.partitionerId
                deliveryType = GuaranteedDelivery
                messageData = c |> SaveChartsPrtMsg
            }.getMessageInfo()
            |> proxy.sendMessage
            |> bindError (addError OnSaveChartsErr (SendChartMessageError (proxy.partitionerId.messagingClientId, c.resultDataId)))
        | NotGeneratedCharts ->
            printfn "onSaveCharts: No charts."
            Ok()


    let onRegister (proxy : OnRegisterProxy) s =
        let result =
            {
                partitionerRecipient = proxy.sendMessageProxy.partitionerId
                deliveryType = GuaranteedDelivery
                messageData = proxy.workerNodeInfo |> RegisterWorkerNodePrtMsg
            }.getMessageInfo()
            |> proxy.sendMessageProxy.sendMessage

        s, result


    let onUnregister (proxy : OnRegisterProxy) s =
        let result =
            {
                partitionerRecipient = proxy.sendMessageProxy.partitionerId
                deliveryType = GuaranteedDelivery
                messageData = proxy.workerNodeInfo.workerNodeId |> UnregisterWorkerNodePrtMsg
            }.getMessageInfo()
            |> proxy.sendMessageProxy.sendMessage

        s, result


    let toDeliveryType progress =
        match progress with
        | NotStarted -> (GuaranteedDelivery, false)
        | InProgress _ -> (NonGuaranteedDelivery, false)
        | Completed _ -> (GuaranteedDelivery, true)
        | Failed _ -> (GuaranteedDelivery, true)
        | Cancelled _ -> (GuaranteedDelivery, true)
        | AllCoresBusy _ -> (GuaranteedDelivery, true)


    let onUpdateProgress (proxy : OnUpdateProgressProxy) s (p : ProgressUpdateInfo) =
        //printfn "onUpdateProgress: runQueueId = %A, progress = %A." p.runQueueId p.progress

        let updateProgress t completed =
            let rso, result =
                match s.runningWorkers |> Map.tryFind p.runQueueId with
                | Some rs ->
                    let result =
                        {
                            partitionerRecipient = proxy.sendMessageProxy.partitionerId
                            deliveryType = t
                            messageData = UpdateProgressPrtMsg p
                        }.getMessageInfo()
                        |> proxy.sendMessageProxy.sendMessage
                        |> bindError (addError OnUpdateProgressErr (UnableToSendProgressMsgErr p.runQueueId))

                    Some { rs with runnerState = { rs.runnerState with progress = p.progress; lastUpdated = DateTime.Now } }, result
                | None -> None, p.runQueueId |> UnableToFindMappingErr |> OnUpdateProgressErr |> WorkerNodeErr |> Error

            if completed
            then
                let r2 = proxy.tryDeleteWorkerNodeRunModelData p.runQueueId
                { s with runningWorkers = s.runningWorkers.tryRemove p.runQueueId }, combineUnitResults result r2
            else
                match rso with
                | Some rs -> { s with runningWorkers = s.runningWorkers.Add(p.runQueueId, rs) }, result
                | None -> s, result

        let (t, completed) = toDeliveryType p.progress
        let w, result = updateProgress t completed
        w, result


    type OnRunModelProxy =
        {
            workerNodeId : WorkerNodeId
            getSolverRunner : WorkerNodeRunModelData -> SolverRunner
            sendMessageProxy : SendMessageProxy
            tryDeleteWorkerNodeRunModelData : RunQueueId -> UnitResult
        }


    let onRunModel (proxy : OnRunModelProxy) (s : WorkerNodeRunnerState) (d : WorkerNodeRunModelData) =
        let w, result =
            match s.numberOfWorkerCores > s.runningWorkers.Count with
            | true ->
                let solver = proxy.getSolverRunner d
                let m = async { solver.runSolver() }
                Async.Start m

                let res =
                    {
                        partitionerRecipient = proxy.sendMessageProxy.partitionerId
                        deliveryType = GuaranteedDelivery
                        messageData = UpdateProgressPrtMsg { runQueueId = d.runningProcessData.runQueueId; progress = InProgress 0.0M }
                    }.getMessageInfo()
                    |> proxy.sendMessageProxy.sendMessage

                { s with runningWorkers = s.runningWorkers.Add(d.runningProcessData.runQueueId, RunnerStateWithCancellation.defaultValue solver.notifyOfResults) }, res
            | false ->
                let res =
                    {
                        partitionerRecipient = proxy.sendMessageProxy.partitionerId
                        deliveryType = GuaranteedDelivery
                        messageData = UpdateProgressPrtMsg { runQueueId = d.runningProcessData.runQueueId; progress = AllCoresBusy proxy.workerNodeId }
                    }.getMessageInfo()
                    |> proxy.sendMessageProxy.sendMessage

                let r2 = proxy.tryDeleteWorkerNodeRunModelData d.runningProcessData.runQueueId
                s, combineUnitResults res r2

        w, result |> bindError (addError OnRunModelErr CannotRunModelErr)


    type OnStartProxy =
        {
            loadAllWorkerNodeRunModelData : unit -> ListResult<WorkerNodeRunModelData>
            onRunModel : WorkerNodeRunnerState -> WorkerNodeRunModelData -> WorkerNodeRunnerResult
            noOfCores : int
        }


    let onStart (proxy : OnStartProxy) s =
        match s.workerNodeState with
        | NotStartedWorkerNode ->
            let w = { s with numberOfWorkerCores = proxy.noOfCores; workerNodeState = StartedWorkerNode }

            let doStart mi =
                let m, mf = mi |> Rop.unzip

                match foldErrors mf with
                | None -> (m, w) ||> Rop.foldWhileOk proxy.onRunModel
                | Some e -> s, Error e

            let g, result =
                match proxy.loadAllWorkerNodeRunModelData() with
                | Ok mi -> doStart mi
                | Error e -> w, Error e

            g, result
        | StartedWorkerNode -> s, Ok()

    type OnProcessMessageProxy =
        {
            saveWorkerNodeRunModelData : WorkerNodeRunModelData -> UnitResult
            tryDeleteWorkerNodeRunModelData : RunQueueId -> UnitResult
            onRunModel : WorkerNodeRunnerState -> WorkerNodeRunModelData -> WorkerNodeRunnerResult
        }


    let onRunModelWrkMsg (proxy : OnProcessMessageProxy) s d m =
        let addError = addError OnProcessMessageErr
        let toError = toError OnProcessMessageErr

        match s.runningWorkers |> Map.tryFind d.runningProcessData.runQueueId with
        | None ->
            match proxy.saveWorkerNodeRunModelData d with
            | Ok() ->
                let w1, r1 = proxy.onRunModel s d

                match r1 with
                | Ok() -> w1, Ok()
                | Error e -> w1, addError (OnRunModelFailedErr (m, d.runningProcessData.runQueueId)) e
            | Error e -> s, addError (CannotSaveModelDataErr (m, d.runningProcessData.runQueueId)) e
        | Some _ -> s, (m, d.runningProcessData.runQueueId) |> ModelAlreadyRunningErr |> toError


    let onCancelRunWrkMsg t s (q, c) =
        //printfn "onCancelRunWrkMsg: Starting: %A ..." q

        let (w, r1) =
            match s.runningWorkers |> Map.tryFind q with
            | Some x -> { s with runningWorkers = s.runningWorkers |> Map.add q { x with cancellationTypeOpt = Some c } }, Ok()
            | None ->
                // kk:20200404 - At this point we don't care if we could not find a running run queue id when trying to cancel it.
                // Otherwise we would have to send a message back that we did not find it and then the caller would have to deal with it!
                // But, ... the model could have been completed in between and we generally don't care about individual models anyway!
                // Anyway, the current view is: if you ask me to cancel but I don't have it, then I just ignore the request.
                s, Ok()

        let result = t q |> combineUnitResults r1
        //printfn "onCancelRunWrkMsg: result: %A" result
        w, result


    let onRequestResultWrkMsg s (q, c) =
        let result =
            match s.runningWorkers |> Map.tryFind q with
            | Some x -> x.notifyOfResults c
            | None -> CannotFindRunQueueErr q |> toError OnRequestResultErr
        s, result


    let onProcessMessage (proxy : OnProcessMessageProxy) s (m : Message) =
        match m.messageData with
        | WorkerNodeMsg x ->
            match x with
            | RunModelWrkMsg d ->
                //printfn "onProcessMessage: RunModelWrkMsg, messageId = %A, runQueueId = %A" m.messageDataInfo.messageId d.runningProcessData.runQueueId
                onRunModelWrkMsg proxy s d m.messageDataInfo.messageId
            | CancelRunWrkMsg q ->
                //printfn "onProcessMessage: CancelRunWrkMsg, messageId = %A, runQueueId = %A" m.messageDataInfo.messageId q
                onCancelRunWrkMsg proxy.tryDeleteWorkerNodeRunModelData s q
            | RequestResultWrkMsg q ->
                printfn "onProcessMessage: RequestResultWrkMsg, messageId = %A, %A" m.messageDataInfo.messageId q
                onRequestResultWrkMsg s q
        | _ -> s, (m.messageDataInfo.messageId, m.messageData.getInfo()) |> InvalidMessageErr |> toError OnProcessMessageErr


    type OnProcessMessageType = OnProcessMessageType<WorkerNodeRunnerState>
    type OnGetMessagesProxy = OnGetMessagesProxy<WorkerNodeRunnerState>
    let onGetMessages = onGetMessages<WorkerNodeRunnerState>
    let onGetState (s : WorkerNodeRunnerState) = s, s.toWorkerNodeRunnerMonitorState() |> WrkNodeState


    type OnConfigureWorkerProxy = OnRegisterProxy


    let onConfigureWorker (proxy : OnConfigureWorkerProxy) (s : WorkerNodeRunnerState) d =
        match d with
        | WorkerNumberOfSores c ->
            let cores = max 0 (min c Environment.ProcessorCount)

            let send() =
                {
                    partitionerRecipient = proxy.sendMessageProxy.partitionerId
                    deliveryType = GuaranteedDelivery
                    messageData = { proxy.workerNodeInfo with noOfCores = cores } |> RegisterWorkerNodePrtMsg
                }.getMessageInfo()
                |> proxy.sendMessageProxy.sendMessage

            let w, result =
                match send() with
                | Ok() -> { s with numberOfWorkerCores = cores }, Ok()
                | Error e -> s, Error e

            w, result


    let onCheckCancellation (s : WorkerNodeRunnerState) q =
        match s.runningWorkers |> Map.tryFind q with
        | Some x -> s, x.cancellationTypeOpt
        | None -> s, None


    let sendMessageProxy i =
        {
            partitionerId = i.workerNodeServiceInfo.workerNodeInfo.partitionerId
            sendMessage = i.messageProcessorProxy.sendMessage
        }


    let onRunModelProxy i p =
        {
            workerNodeId = i.workerNodeServiceInfo.workerNodeInfo.workerNodeId
            getSolverRunner = getSolverRunner p
            sendMessageProxy = sendMessageProxy i
            tryDeleteWorkerNodeRunModelData = i.workerNodeProxy.tryDeleteWorkerNodeRunModelData
        }


    let onRegisterProxy i : OnRegisterProxy =
        {
            workerNodeInfo = i.workerNodeServiceInfo.workerNodeInfo
            sendMessageProxy = sendMessageProxy i
        }


    let onUpdateProgressProxy i =
        {
            tryDeleteWorkerNodeRunModelData = i.workerNodeProxy.tryDeleteWorkerNodeRunModelData
            sendMessageProxy = sendMessageProxy i
        }


    let onProcessMessageProxy i p =
        {
            saveWorkerNodeRunModelData = i.workerNodeProxy.saveWorkerNodeRunModelData
            tryDeleteWorkerNodeRunModelData = i.workerNodeProxy.tryDeleteWorkerNodeRunModelData
            onRunModel = onRunModel (onRunModelProxy i p)
        }


    type WorkerNodeMessage =
        | Start of OnStartProxy * AsyncReplyChannel<UnitResult>
        | Register of AsyncReplyChannel<UnitResult>
        | Unregister of AsyncReplyChannel<UnitResult>
        | UpdateProgress of AsyncReplyChannel<UnitResult> * ProgressUpdateInfo
        | SaveResult of AsyncReplyChannel<UnitResult> * ResultDataWithId
        | SaveCharts of AsyncReplyChannel<UnitResult> * ChartGenerationResult
        | GetMessages of OnGetMessagesProxy * AsyncReplyChannel<UnitResult>
        | GetState of AsyncReplyChannel<WorkerNodeMonitorResponse>
        | ConfigureWorker of AsyncReplyChannel<UnitResult> * WorkerNodeConfigParam
        | CheckCancellation of AsyncReplyChannel<CancellationType option> * RunQueueId


    type WorkerNodeRunner(i : WorkerNodeRunnerData) =
        let onRegisterProxy = onRegisterProxy i
        let onUpdateProgressProxy = onUpdateProgressProxy i
        let onConfigureWorkerProxy = onRegisterProxy
        let sendMessageProxy = sendMessageProxy i

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start (p, r) -> return! onStart p s |> (withReply r) |> loop
                            | Register r -> return! onRegister onRegisterProxy s |> (withReply r) |> loop
                            | Unregister r -> return! onUnregister onRegisterProxy s |> (withReply r) |> loop
                            | UpdateProgress (r, p) -> return! onUpdateProgress onUpdateProgressProxy s p |> (withReply r) |> loop
                            | SaveResult (r, p) -> return! (s, onSaveResult sendMessageProxy p) |> (withReply r) |> loop
                            | SaveCharts (r, c) -> return! (s, onSaveCharts sendMessageProxy c) |> (withReply r) |> loop
                            | GetMessages (p, r) -> return! onGetMessages p s |> (withReply r) |> loop
                            | GetState r -> return! onGetState s |> (withReply r) |> loop
                            | ConfigureWorker (r, d) -> return! onConfigureWorker onConfigureWorkerProxy s d |> (withReply r) |> loop
                            | CheckCancellation (r, q) -> return! onCheckCancellation s q |> (withReply r) |> loop
                        }

                WorkerNodeRunnerState.defaultValue |> loop
                )

        member w.start() = messageLoop.PostAndReply (fun reply -> Start (w.onStartProxy, reply))
        member _.register() = messageLoop.PostAndReply Register
        member _.unregister() = messageLoop.PostAndReply Unregister
        member _.updateProgress p = messageLoop.PostAndReply (fun r -> UpdateProgress (r, p))
        member _.saveResult p = messageLoop.PostAndReply (fun r -> SaveResult (r, p))
        member _.saveCharts p = messageLoop.PostAndReply (fun r -> SaveCharts (r, p))
        member w.getMessages() = messageLoop.PostAndReply (fun reply -> GetMessages (w.onGetMessagesProxy, reply))
        member _.getState () = messageLoop.PostAndReply GetState
        member _.configure d = messageLoop.PostAndReply (fun r -> ConfigureWorker (r, d))
        member _.checkCancellation q = messageLoop.PostAndReply (fun r -> CheckCancellation (r, q))

        member w.solverRunnerProxy =
            {
                updateProgress = w.updateProgress
                saveResult = w.saveResult
                saveCharts = w.saveCharts
                logCrit = i.workerNodeProxy.logCrit
                checkCancellation = w.checkCancellation
            }

        member w.onStartProxy =
            {
                loadAllWorkerNodeRunModelData = i.workerNodeProxy.loadAllWorkerNodeRunModelData
                onRunModel = onRunModel (onRunModelProxy i w.solverRunnerProxy)
                noOfCores = i.workerNodeServiceInfo.workerNodeInfo.noOfCores
            }

        member w.onGetMessagesProxy =
            {
                tryProcessMessage = onTryProcessMessage i.messageProcessorProxy
                onProcessMessage = onProcessMessage (onProcessMessageProxy i w.solverRunnerProxy)
                maxMessages = WorkerNodeRunnerState.maxMessages
                onError = fun f -> f |> OnGetMessagesErr |> WorkerNodeErr
            }


    let createServiceImpl (logger : Logger) (i : WorkerNodeRunnerData) =
        logger.logInfoString "createServiceImpl: Creating WorkerNodeRunner..."
        let w = WorkerNodeRunner i

        match i.workerNodeServiceInfo.workerNodeInfo.isInactive with
        | false ->
            logger.logInfoString "createServiceImpl: Registering..."
            match w.register >-> w.start |> evaluate with
            | Ok() ->
                let h = ClmEventHandler(ClmEventHandlerInfo.defaultValue logger w.getMessages "WorkerNodeRunner - getMessages")
                do h.start()
                Ok w
            | Error e -> Error e
        | true ->
            logger.logInfoString "createServiceImpl: Unregistering..."
            match w.unregister() with
            | Ok() -> failwith "createServiceImpl for inactive worker node is not implemented yet."
            | Error e -> Error e


    type WorkerNodeRunner
        with
        static member create (j : ClmResult<WorkerNodeServiceInfo>) =
            let logger = Logger.log4net
            let className = "WorkerNodeService"
            let toError e = e |> WorkerNodeServiceErr |> Error
            let addError f e = ((f |> WorkerNodeServiceErr) + e) |> Error

            let msgDbLocation = getFileName MsgDatabase
            let connStrSqlite = @"Data Source=" + msgDbLocation + ";Version=3;foreign keys=true"
            logger.logInfoString (sprintf "%s: Using local database: '%s'." className msgDbLocation)
            
            match j with
            | Ok i ->
                let w =
                    let messagingClientAccessInfo = i.messagingClientAccessInfo
                    let h = MsgResponseHandler messagingClientAccessInfo
                    logger.logInfoString (sprintf "%s: Created MsgResponseHandler: %A" className h)

                    let j =
                        {
                            messagingClientName = workerNodeServiceName.value.messagingClientName
                            storageType = connStrSqlite |> SqliteConnectionString |> SqliteDatabase
                        }

                    let messagingClientData =
                        {
                            msgAccessInfo = messagingClientAccessInfo
                            messagingService = h
                            msgClientProxy = MessagingClientProxy.create j messagingClientAccessInfo.msgClientId
                            expirationTime = MessagingClientData.defaultExpirationTime
                        }

                    let messagingClient = MessagingClient messagingClientData

                    match messagingClient.start() with
                    | Ok() ->
                        let n =
                            {
                                workerNodeServiceInfo = i
                                workerNodeProxy = WorkerNodeProxy.create WorkerNodeProxyData.defaultValue
                                messageProcessorProxy = messagingClient.messageProcessorProxy
                                minUsefulEe = MinUsefulEe.defaultValue
                            }
                            |> createServiceImpl logger

                        match n with
                        | Ok v ->
                            createMessagingClientEventHandlers logger messagingClient.messageProcessorProxy
                            Ok v
                        | Error e -> addError UnableToCreateWorkerNodeServiceErr e
                    | Error e -> addError UnableToStartMessagingClientErr e

                w
            | Error e -> Error e                
