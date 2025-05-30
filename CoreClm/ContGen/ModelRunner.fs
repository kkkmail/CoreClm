﻿namespace ContGen

//open Primitives.GeneralPrimitives
//open Primitives.SolverPrimitives
open Softellect.Sys.Rop
open Softellect.Messaging.Primitives
open Softellect.Messaging.Client

open Clm.ModelParams
open Softellect.Messaging.ServiceInfo
open ContGenServiceInfo.ServiceInfo
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
//open ClmSys.TimerEvents
//open ClmSys.WorkerNodeData
//open ServiceProxy.ModelRunnerProxy
//open ClmSys.ModelRunnerErrors
open MessagingServiceInfo.ServiceInfo
//open ClmSys.WorkerNodePrimitives
//open ClmSys.Logging
open DbData.DatabaseTypesDbo
open DbData.DatabaseTypesClm
//open ServiceProxy.MsgProcessorProxy
open Softellect.Messaging.Errors
open ModelGenerator

module ModelRunner =
    let x = 1

//    let private printDebug s = printfn $"{s}"
////    let private printDebug s = ()

//    let private toError g f = f |> g |> ModelRunnerErr |> Error
//    let private addError g f e = ((f |> g |> ModelRunnerErr) + e) |> Error
//    let private maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]

//    type OnProcessMessageType = OnProcessMessageType<unit>
//    type OnGetMessagesProxy = OnGetMessagesProxy<unit>
//    let onGetMessages = onGetMessages<unit>


//    let runModel (proxy : RunModelProxy) (q : RunQueue) : UnitResult =
//        match q.toMessageInfoOpt proxy.loadModelData proxy.controlData with
//        | Ok (Some m) ->
//            match proxy.sendRunModelMessage m with
//            | Ok v -> Ok v
//            | Error e -> MessagingRunnerErr e |> toError RunModelRunnerErr
//        | Ok None -> q.runQueueId |> MissingWorkerNodeRunnerErr |> toError RunModelRunnerErr
//        | Error e -> (addError RunModelRunnerErr) (UnableToLoadModelDataRunnerErr (q.runQueueId, q.info.modelDataId )) e


//    /// Tries to run the first not scheduled run queue entry using the first available worker node.
//    let tryRunFirstModel (proxy : TryRunFirstModelProxy) =
//        let addError = addError TryRunFirstModelRunnerErr

//        match proxy.tryLoadFirstRunQueue() with
//        | Ok (Some q) ->
//            match proxy.tryGetAvailableWorkerNode() with
//            | Ok (Some n) ->
//                let q1 = { q with workerNodeIdOpt = Some n; runQueueStatus = RunRequestedRunQueue }

//                match proxy.upsertRunQueue q1 with
//                | Ok() ->
//                    match proxy.runModel q1 with
//                    | Ok() -> Ok WorkScheduled
//                    | Error e ->
//                        match proxy.upsertRunQueue { q1 with runQueueStatus = FailedRunQueue } with
//                        | Ok() -> addError UnableToRunModelRunnerErr e
//                        | Error f -> addError UnableToRunModelAndUpsertStatusRunnerErr (f + e)
//                | Error e -> addError UpsertRunQueueRunnerErr e
//            | Ok None -> Ok NoAvailableWorkerNodes
//            | Error e -> addError UnableToGetWorkerNodeRunnerErr e
//        | Ok None -> Ok NoWork
//        | Error e -> addError TryLoadFirstRunQueueRunnerErr e


//    let tryCancelRunQueue (proxy : TryCancelRunQueueProxy) (q, c) =
//        let addError = addError TryCancelRunQueueRunnerErr
//        let toError = toError TryCancelRunQueueRunnerErr

//        match proxy.tryLoadRunQueue q with
//        | Ok (Some r) ->
//            let r1 =
//                match r.workerNodeIdOpt with
//                | Some w ->
//                    let r11 =
//                        {
//                            recipientInfo =
//                                {
//                                    recipient = w.messagingClientId
//                                    deliveryType = GuaranteedDelivery
//                                }

//                            messageData = (q, c) |> CancelRunWrkMsg |> WorkerNodeMsg |> UserMsg
//                        }
//                        |> proxy.sendCancelRunQueueMessage

//                    match r11 with
//                    | Ok v -> Ok v
//                    | Error e -> TryCancelRunQueueRunnerError.MessagingTryCancelRunQueueRunnerErr e |> toError
//                | None -> Ok()

//            let r2 =
//                match r.runQueueStatus with
//                | NotStartedRunQueue -> { r with runQueueStatus = CancelledRunQueue } |> proxy.upsertRunQueue
//                | RunRequestedRunQueue -> { r with runQueueStatus = CancelRequestedRunQueue } |> proxy.upsertRunQueue
//                | InProgressRunQueue -> { r with runQueueStatus = CancelRequestedRunQueue } |> proxy.upsertRunQueue
//                | CancelRequestedRunQueue -> { r with runQueueStatus = CancelRequestedRunQueue } |> proxy.upsertRunQueue
//                | _ -> q |> TryCancelRunQueueRunnerError.InvalidRunQueueStatusRunnerErr |> toError

//            combineUnitResults r1 r2
//        | Ok None -> toError (TryCancelRunQueueRunnerError.TryLoadRunQueueRunnerErr q)
//        | Error e -> addError (TryCancelRunQueueRunnerError.TryLoadRunQueueRunnerErr q) e


//    let tryRequestResults (proxy : TryRequestResultsProxy) (q, c) =
//        let addError = addError TryRequestResultsRunnerErr
//        let toError = toError TryRequestResultsRunnerErr

//        match proxy.tryLoadRunQueue q with
//        | Ok (Some r) ->
//            match r.workerNodeIdOpt with
//            | Some w ->
//                let r1 =
//                    {
//                        recipientInfo =
//                            {
//                                recipient = w.messagingClientId
//                                deliveryType = GuaranteedDelivery
//                            }

//                        messageData = (q, c) |> RequestResultWrkMsg |> WorkerNodeMsg |> UserMsg
//                    }
//                    |> proxy.sendRequestResultsMessage

//                match r1 with
//                | Ok v -> Ok v
//                | Error e -> MessagingTryRequestResultsRunnerErr e |> toError
//            | None -> Ok()
//        | Ok None -> toError (TryRequestResultsRunnerError.TryLoadRunQueueRunnerErr q)
//        | Error e -> addError (TryRequestResultsRunnerError.TryLoadRunQueueRunnerErr q) e


//    let tryReset (proxy : TryResetProxy) q =
//        proxy.tryResetRunQueue q


//    /// Tries to run all available work items (run queue) on all available work nodes until one or the other is exhausted.
//    let tryRunAllModels (proxy : TryRunAllModelsProxy) =
//        let rec doWork() =
//            match proxy.tryRunFirstModel() with
//            | Ok r ->
//                match r with
//                | WorkScheduled -> doWork()
//                | NoWork -> Ok()
//                | NoAvailableWorkerNodes -> Ok()
//            | Error e -> addError TryRunAllModelsRunnerErr UnableToTryRunFirstModelRunnerErr e

//        doWork()


//    let updateProgress (proxy : UpdateProgressProxy) (i : ProgressUpdateInfo) =
//        printDebug $"updateProgress: i = %A{i}"
//        let addError = addError UpdateProgressRunnerErr
//        let toError = toError UpdateProgressRunnerErr

//        match proxy.tryLoadRunQueue i.runQueueId with
//        | Ok (Some q) ->
//            let q1 = { q with progressData = i.progressData }

//            let upsert q2 =
//                printfn $"updateProgress.upsert: Upserting %A{i} into %A{q2}."

//                match proxy.upsertRunQueue q2 with
//                | Ok() -> Ok()
//                | Error e -> addError (UnableToLoadRunQueueRunnerErr i.runQueueId) e
//                // (r, addError (UnableToLoadRunQueueErr i.runQueueId) e) ||> combineUnitResults

//            match i.updatedRunQueueStatus with
//            | Some s -> { q1 with runQueueStatus = s }
//            | None -> q1
//            |> upsert
//        | Ok None -> toError (UnableToFindLoadRunQueueRunnerErr i.runQueueId)
//        | Error e -> addError (UnableToLoadRunQueueRunnerErr i.runQueueId) e


//    let register (proxy : RegisterProxy) (r : WorkerNodeInfo) =
//        //printfn "register: r = %A" r
//        proxy.upsertWorkerNodeInfo r |> bindError (addError RegisterRunnerErr (UnableToUpsertWorkerNodeInfoRunnerErr r.workerNodeId))


//    let unregister (proxy : UnregisterProxy) (r : WorkerNodeId) =
//        //printfn "unregister: r = %A" r
//        let addError = addError UnregisterRunnerErr

//        match proxy.loadWorkerNodeInfo r with
//        | Ok w -> proxy.upsertWorkerNodeInfo { w with noOfCores = 0 } |> bindError (addError (UnableToUpsertWorkerNodeInfoOnUnregisterRunnerErr r))
//        | Error e -> addError (UnableToLoadWorkerNodeInfoRunnerErr r) e


//    let saveCharts (proxy : SaveChartsProxy) (c : ChartInfo) =
//        printfn $"saveCharts: c.runQueueId = %A{c.runQueueId}"
//        proxy.saveCharts c |> bindError (addError SaveChartsRunnerErr (UnableToSaveChartsRunnerErr c.runQueueId))


//    let processMessage (proxy : ProcessMessageProxy) (m : Message) =
//        printfn $"processMessage: messageId = %A{m.messageDataInfo.messageId}, message = %A{m}."

//        let r =
//            match m.messageData with
//            | UserMsg (PartitionerMsg x) ->
//                match x with
//                | UpdateProgressPrtMsg i -> proxy.updateProgress i
//                | SaveChartsPrtMsg c -> proxy.saveCharts c
//                | RegisterWorkerNodePrtMsg r -> proxy.register r
//                | UnregisterWorkerNodePrtMsg r -> proxy.unregister r
//                |> bindError (addError ProcessMessageRunnerErr (ErrorWhenProcessingMessageRunnerErr m.messageDataInfo.messageId))
//            | _ -> toError ProcessMessageRunnerErr (InvalidMessageTypeRunnerErr m.messageDataInfo.messageId)

//        match r with
//        | Ok() -> ()
//        | Error e -> printfn $"processMessage: messageId = %A{m.messageDataInfo.messageId}, ERROR = %A{e}."

//        r


//    let getRunState (proxy : GetRunStateProxy) =
//        let w, e = proxy.loadRunQueueProgress() |> unzipListResult
//        w, e |> foldToUnitResult


    //type ProcessMessageProxy
    //    with
    //    static member create c p resultLocation =
    //        {
    //            updateProgress = updateProgress (UpdateProgressProxy.create c p)
    //            saveCharts = saveCharts (SaveChartsProxy.create resultLocation)
    //            register = register (RegisterProxy.create c)
    //            unregister = unregister (UnregisterProxy.create c p)
    //        }


    //type TryRunFirstModelProxy
    //    with
    //    static member create c rmp m =
    //        {
    //            tryLoadFirstRunQueue = fun () -> tryLoadFirstRunQueue c
    //            tryGetAvailableWorkerNode = fun () -> tryGetAvailableWorkerNode c m
    //            runModel = runModel rmp
    //            upsertRunQueue = upsertRunQueue c
    //        }


    //type TryCancelRunQueueProxy
    //    with
    //    static member create c s =
    //        {
    //            tryLoadRunQueue = tryLoadRunQueue c
    //            sendCancelRunQueueMessage = s
    //            upsertRunQueue = upsertRunQueue c
    //        }


    //type TryRequestResultsProxy
    //    with
    //    static member create c s =
    //        {
    //            tryLoadRunQueue = tryLoadRunQueue c
    //            sendRequestResultsMessage = s
    //        }


    //type TryResetProxy
    //    with
    //    static member create c =
    //        {
    //            tryResetRunQueue = tryResetRunQueue c
    //        }


    //type TryRunAllModelsProxy
    //    with
    //    static member create c r m =
    //        {
    //            tryRunFirstModel = fun () -> tryRunFirstModel (TryRunFirstModelProxy.create c r m)
    //        }


    //let onGetMessagesProxy c p resultLocation w : OnGetMessagesProxy<unit> =
    //    let proxy = ProcessMessageProxy.create c p resultLocation

    //    // TODO kk:20240722 - Deelevation happens here. This is not good.
    //    // See: https://github.com/kkkmail/CoreClm/issues/40
    //    let p () m =
    //        let r =
    //            match processMessage proxy m with
    //            | Ok v -> Ok v
    //            | Error e ->
    //                printfn $"onGetMessagesProxy - error: '{e}'."
    //                OnGetMessagesErr FailedToProcessErr |> Error
    //        (),r

    //    {
    //        tryProcessMessage = onTryProcessMessage w
    //        onProcessMessage = p
    //        maxMessages = maxMessages
    //    }


    //type RunnerMessage =
    //    | TryRunAll of AsyncReplyChannel<UnitResult>
    //    | TryCancelRunQueue of AsyncReplyChannel<UnitResult> * RunQueueId * CancellationType
    //    | TryRequestResults of AsyncReplyChannel<UnitResult> * RunQueueId * ResultNotificationType
    //    | TryReset of AsyncReplyChannel<UnitResult> * RunQueueId
    //    | ProcessMessages of AsyncReplyChannel<MessagingUnitResult>


    //type Runner (i : RunnerDataWithProxy) =
    //    let c = i.runnerData.contGenInfo
    //    let runModelProxy = RunModelProxy.create i.runnerData i.messageProcessorProxy.sendMessage
    //    let tryRunAllModelsProxy = TryRunAllModelsProxy.create i.runnerData.getConnectionString runModelProxy c.lastAllowedNodeErr
    //    let tryCancelRunQueueProxy = TryCancelRunQueueProxy.create i.runnerData.getConnectionString i.messageProcessorProxy.sendMessage
    //    let tryRequestResultsProxy = TryRequestResultsProxy.create i.runnerData.getConnectionString i.messageProcessorProxy.sendMessage
    //    let tryResetProxy : TryResetProxy = TryResetProxy.create i.runnerData.getConnectionString
    //    let proxy = onGetMessagesProxy i.runnerData.getConnectionString c.partitionerId c.resultLocation i.messageProcessorProxy

    //    let messageLoop =
    //        MailboxProcessor.Start(fun u ->
    //            let rec loop() =
    //                async
    //                    {
    //                        match! u.Receive() with
    //                        | TryRunAll r -> tryRunAllModels tryRunAllModelsProxy |> r.Reply
    //                        | TryCancelRunQueue (r, q, c) -> tryCancelRunQueue tryCancelRunQueueProxy (q, c) |> r.Reply
    //                        | TryRequestResults (r, q, c) -> tryRequestResults tryRequestResultsProxy (q, c) |> r.Reply
    //                        | TryReset (r, q) -> tryReset tryResetProxy q |> r.Reply
    //                        | ProcessMessages r -> onGetMessages proxy () |> snd |> r.Reply

    //                        return! loop()
    //                    }

    //            loop()
    //            )

    //    member _.tryRunAll() = messageLoop.PostAndReply (fun reply -> TryRunAll reply)
    //    member _.tryCancelRunQueue q c = messageLoop.PostAndReply (fun reply -> TryCancelRunQueue (reply, q, c))
    //    member _.processMessages() = messageLoop.PostAndReply (fun reply -> ProcessMessages reply)
    //    member _.tryRequestResults q c = messageLoop.PostAndReply (fun reply -> TryRequestResults (reply, q, c))
    //    member _.tryReset q = messageLoop.PostAndReply (fun reply -> TryReset (reply, q))


    //let createModelRunner (logger : Logger) (r : Runner) =
    //    logger.logInfoString "createModelRunner: Creating model runner..."
    //    let e = fun () -> r.tryRunAll()
    //    let h = ClmEventHandler(ClmEventHandlerInfo.defaultValue logger e "ModelRunner - tryRunAllModels")
    //    h


    ///// Call this function to create timer events necessary for automatic ModelRunner operation.
    ///// If you don't call it, then you have to operate it by hands.
    //let createModelRunnerMessageProcessor (logger : Logger) (r : Runner) =
    //    logger.logInfoString "createModelRunnerMessageProcessor: Creating message processor..."

    //    let e : (unit -> UnitResult) = fun () ->
    //        match r.processMessages() with
    //        | Ok v -> Ok v
    //        | Error e -> CreateModelRunnerMessageProcessorErr e |> Error

    //    let h = ClmEventHandler(ClmEventHandlerInfo.defaultValue logger e "ModelRunnerMessageProcessor - onGetMessages")
    //    h


    //type ModelRunner =
    //    {
    //        modelGenerator : ClmEventHandler
    //        modelRunner : ClmEventHandler
    //        tryCancelRunQueue : RunQueueId -> CancellationType -> UnitResult
    //        tryRequestResults : RunQueueId -> ResultNotificationType -> UnitResult
    //        tryReset : RunQueueId -> UnitResult
    //        messageProcessor : ClmEventHandler
    //    }

    //    member p.start() =
    //        do
    //            p.modelGenerator.start()
    //            p.modelRunner.start()
    //            p.messageProcessor.start()

    //    member p.stop() =
    //        do
    //            p.modelGenerator.stop()
    //            p.modelRunner.stop()
    //            p.messageProcessor.stop()

    //    static member create so (d : ModelRunnerDataWithProxy) =
    //        let messagingClient = d.runnerProxy.getMessageProcessorProxy d.messagingClientAccessInfo
    //        let c = d.runnerData.contGenInfo

    //        match messagingClient.start() with
    //        | Ok() ->
    //            let data =
    //                {
    //                    runnerData = d.runnerData
    //                    messageProcessorProxy = messagingClient
    //                }

    //            let runner = Runner(data)

    //            {
    //                modelGenerator = createModelGenerator d.logger c.dictionaryUpdateType c.collisionData so d.runnerData.getConnectionString
    //                modelRunner = createModelRunner d.logger runner
    //                tryCancelRunQueue = runner.tryCancelRunQueue
    //                tryRequestResults = runner.tryRequestResults
    //                tryReset = runner.tryReset
    //                messageProcessor = createModelRunnerMessageProcessor d.logger runner
    //            }
    //            |> Ok
    //        | Error e -> Error e


    type ModelMonitor =
        {
            getRunState : unit -> (list<RunQueue> * UnitResult)
        }

        static member create connectionString =
            let proxy = GetRunStateProxy.create connectionString
            let g() = getRunState proxy

            {
                getRunState = g
            }
