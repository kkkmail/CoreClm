namespace WorkerNodeService

open System
open Argu

open Softellect.Sys.Core
open Softellect.Sys
open Softellect.Messaging.ServiceInfo
open Softellect.Wcf.Common
open Softellect.Messaging.Primitives
open Softellect.Messaging.Client
open Softellect.Messaging.Proxy
open Softellect.Sys.Rop

open ClmSys.Logging
open ClmSys.WorkerNodeData
open ClmSys.TimerEvents
open ContGenServiceInfo.ServiceInfo
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.SvcCommandLine
open ServiceProxy.MsgServiceProxy
open MessagingServiceInfo.ServiceInfo
open Clm.ModelParams
open ServiceProxy.WorkerNodeProxy
open ServiceProxy.MsgProcessorProxy
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.WorkerNodeErrors
open ClmSys.WorkerNodePrimitives
open ServiceProxy.SolverRunner
open SolverRunner.SolverRunnerTasks
open ClmSys.SolverRunnerPrimitives
open ClmSys.MessagingData
open ServiceProxy.SolverProcessProxy
open DbData.Configuration
open DbData.WorkerNodeDatabaseTypes

module ServiceImplementation =

    let private toError g f = f |> g |> WorkerNodeErr |> Error
    let private addError g f e = ((f |> g |> WorkerNodeErr) + e) |> Error


    let mutable serviceAccessInfo =
        let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = workerNodeServiceProgramName)
        let results = (parser.Parse [||]).GetAllResults()
        results |> getServiceAccessInfo


    type WorkerNodeMessageResult = MessageProcessorResult<WorkerNodeRunnerState * UnitResult, ClmError>


    type WorkerNodeRunnerState
        with
        static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]

        static member defaultValue =
            {
                workerNodeState = NotStartedWorkerNode
            }


    type WorkerNodeRunnerData =
        {
            workerNodeServiceInfo : WorkerNodeServiceInfo
            workerNodeProxy : WorkerNodeProxy
            messageProcessorProxy : MessageProcessorProxy
            minUsefulEe : MinUsefulEe
        }


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


//    let onRunModel (proxy : OnRunModelProxy) q =
//        let failed p =
//            {
//                partitionerRecipient = proxy.sendMessageProxy.partitionerId
//                deliveryType = GuaranteedDelivery
//                messageData = UpdateProgressPrtMsg { runQueueId = q; progress = p }
//            }.getMessageInfo()
//            |> proxy.sendMessageProxy.sendMessage
//
//        match proxy.tryGetRunningSolversCount() with
//        | Ok n ->
//            match n < proxy.numberOfWorkerCores with
//            | true -> proxy.runSolver q
//            | false -> AllCoresBusy proxy.workerNodeId |> failed
//        | Error e -> Failed (ErrorMessage $"{e}") |> failed


    let onStart (proxy : OnStartProxy) s =
        match s.workerNodeState with
        | NotStartedWorkerNode ->
            let w = { s with workerNodeState = StartedWorkerNode }

            let doStart (mi : list<ClmResult<RunQueueId>>) : UnitResult =
                let m, mf = mi |> Rop.unzip

                let r =
                    (m |> List.map proxy.onRunModel)
                    @
                    [ foldErrors mf |> toUnitResult ]
                    |> foldUnitResults
                r

            let result = proxy.loadAllActiveRunQueueId() >>= doStart
            w, result
        | StartedWorkerNode -> s, Ok()


    let onProcessMessage (proxy : OnProcessMessageProxy) (m : Message) =
        match m.messageData with
        | UserMsg (WorkerNodeMsg x) ->
            match x with
            | RunModelWrkMsg d ->
                match proxy.saveWorkerNodeRunModelData d with
                | Ok() -> proxy.onRunModel d.runningProcessData.runQueueId
                | Error e -> addError OnProcessMessageErr (CannotSaveModelDataErr (m.messageDataInfo.messageId, d.runningProcessData.runQueueId)) e
            | CancelRunWrkMsg q -> q ||> proxy.requestCancellation
            | RequestResultWrkMsg q -> q ||> proxy.notifyOfResults
        | _ -> (m.messageDataInfo.messageId, m.messageData.getInfo()) |> InvalidMessageErr |> toError OnProcessMessageErr


//    type OnProcessMessageType = OnProcessMessageType<unit>
    type OnGetMessagesProxy = OnGetMessagesProxy<WorkerNodeRunnerState>
    let private onGetMessages = onGetMessages<WorkerNodeRunnerState>
    type OnConfigureWorkerProxy = OnRegisterProxy

    let onGetState (s : WorkerNodeRunnerState) =
        failwith ""
        // s, s.toWorkerNodeRunnerMonitorState() |> WrkNodeState


//    let onConfigureWorker (proxy : OnConfigureWorkerProxy) (s : WorkerNodeRunnerState) d =
//        match d with
//        | WorkerNumberOfSores c ->
//            let cores = max 0 (min c (2 * Environment.ProcessorCount))
//
//            let send() =
//                {
//                    partitionerRecipient = proxy.sendMessageProxy.partitionerId
//                    deliveryType = GuaranteedDelivery
//                    messageData = { proxy.workerNodeInfo with noOfCores = cores } |> RegisterWorkerNodePrtMsg
//                }.getMessageInfo()
//                |> proxy.sendMessageProxy.sendMessage
//
//            let w, result =
//                match send() with
//                | Ok() -> { s with numberOfWorkerCores = cores }, Ok()
//                | Error e -> s, Error e
//
//            w, result


    let sendMessageProxy i =
        {
            partitionerId = i.workerNodeServiceInfo.workerNodeInfo.partitionerId
            sendMessage = i.messageProcessorProxy.sendMessage
        }


    let onRegisterProxy i : OnRegisterProxy =
        {
            workerNodeInfo = i.workerNodeServiceInfo.workerNodeInfo
            sendMessageProxy = sendMessageProxy i
        }


//    let onUpdateProgressProxy i =
//        {
//            tryDeleteWorkerNodeRunModelData = i.workerNodeProxy.tryDeleteWorkerNodeRunModelData
//            sendMessageProxy = sendMessageProxy i
//        }


//    let onProcessMessageProxy i p =
//        {
//            saveWorkerNodeRunModelData = i.workerNodeProxy.saveWorkerNodeRunModelData
//            requestCancellation = i.workerNodeProxy.tryDeleteWorkerNodeRunModelData
//            notifyOfResults = 0
//            onRunModel = onRunModel (onRunModelProxy i p)
//        }


    type WorkerNodeMessage =
        | Start of OnStartProxy * AsyncReplyChannel<UnitResult>
        | Register of AsyncReplyChannel<UnitResult>
        | Unregister of AsyncReplyChannel<UnitResult>
        | GetMessages of OnGetMessagesProxy * AsyncReplyChannel<UnitResult>
        | GetState of AsyncReplyChannel<WorkerNodeMonitorResponse>
//        | ConfigureWorker of AsyncReplyChannel<UnitResult> * WorkerNodeConfigParam


    type WorkerNodeRunner(i : WorkerNodeRunnerData) =
        let onRegisterProxy = onRegisterProxy i
//        let onUpdateProgressProxy = onUpdateProgressProxy i
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
                            | GetMessages (p, r) -> return! onGetMessages p s |> (withReply r) |> loop
                            | GetState r -> return! onGetState s |> (withReply r) |> loop
//                            | ConfigureWorker (r, d) -> return! onConfigureWorker onConfigureWorkerProxy s d |> (withReply r) |> loop
                        }

                WorkerNodeRunnerState.defaultValue |> loop
                )

        member w.start() = messageLoop.PostAndReply (fun reply -> Start (w.onStartProxy, reply))
        member _.register() = messageLoop.PostAndReply Register
        member _.unregister() = messageLoop.PostAndReply Unregister
        member w.getMessages() = messageLoop.PostAndReply (fun reply -> GetMessages (w.onGetMessagesProxy, reply))
        member _.getState() = messageLoop.PostAndReply GetState
//        member _.configure d = messageLoop.PostAndReply (fun r -> ConfigureWorker (r, d))

        member w.onStartProxy =
            {
                loadAllActiveRunQueueId = i.workerNodeProxy.loadAllActiveRunQueueId
                onRunModel = i.workerNodeProxy.onProcessMessageProxy.onRunModel
            }

        member w.onGetMessagesProxy =
            {
                tryProcessMessage = onTryProcessMessage i.messageProcessorProxy
                onProcessMessage = fun w m -> w, onProcessMessage i.workerNodeProxy.onProcessMessageProxy m
                maxMessages = WorkerNodeRunnerState.maxMessages
                onError = fun f -> f |> OnGetMessagesErr |> WorkerNodeErr
                addError = fun a b -> a + b
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
            let logger = Logger.defaultValue
            let addError f e = ((f |> WorkerNodeServiceErr) + e) |> Error
            let c = getWorkerNodeSvcConnectionString

            match j with
            | Ok i ->
                let w =
                    let messagingClientAccessInfo = i.messagingClientAccessInfo

                    let j =
                        {
                            messagingClientName = WorkerNodeServiceName.netTcpServiceName.value.value |> MessagingClientName
                            storageType = c |> MsSqlDatabase
                        }

                    let messagingClientData =
                        {
                            msgAccessInfo = messagingClientAccessInfo
                            communicationType = NetTcpCommunication
                            msgClientProxy = createMessagingClientProxy j messagingClientAccessInfo.msgClientId
                            expirationTime = MessagingClientData.defaultExpirationTime
                        }

                    let messagingClient = MessagingClient messagingClientData

                    match messagingClient.start() with
                    | Ok() ->
                        let n =
                            {
                                workerNodeServiceInfo = i
                                workerNodeProxy = WorkerNodeProxy.create (failwith "") (failwith "")
                                messageProcessorProxy = messagingClient.messageProcessorProxy
                                minUsefulEe = MinUsefulEe.defaultValue
                            }
                            |> createServiceImpl logger

                        match n with
                        | Ok v -> Ok v
                        | Error e -> addError UnableToCreateWorkerNodeServiceErr e
                    | Error e -> addError UnableToStartMessagingClientErr e

                w
            | Error e -> Error e
