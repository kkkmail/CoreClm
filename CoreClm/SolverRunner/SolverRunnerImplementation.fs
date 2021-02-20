namespace SolverRunner

open Argu
open Clm.ModelParams
open ClmSys.ClmErrors
open ClmSys.ExitErrorCodes
open ClmSys.GeneralPrimitives
open ServiceProxy.SolverProcessProxy
open ServiceProxy.WorkerNodeProxy
open SolverRunner.SolverRunnerCommandLine
open NoSql.FileSystemTypes
open System
open Argu
open DbData.Configuration
open DbData.WorkerNodeDatabaseTypes

open Softellect.Sys.Core
open Softellect.Sys
open Softellect.Messaging.ServiceInfo
open Softellect.Wcf.Common
open Softellect.Messaging.Primitives
open Softellect.Messaging.Client
open Softellect.Messaging.Proxy

open ClmSys.Logging
open ClmSys.WorkerNodeData
open ClmSys.TimerEvents
open ContGenServiceInfo.ServiceInfo
open WorkerNodeServiceInfo.ServiceInfo
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
open ClmSys.SolverRunnerErrors
open DbData.MsgSvcDatabaseTypes
open System
open System.Diagnostics
open ClmSys.GeneralPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.SolverRunnerPrimitives
open ClmSys.ClmErrors
open ClmSys.VersionInfo
open MessagingServiceInfo.ServiceInfo
open DbData.WorkerNodeDatabaseTypes
open DbData.Configuration
open DbData.MsgSvcDatabaseTypes
open Softellect.Messaging.Client

module SolverRunnerImplementation =

    let private toError g f = f |> g |> SolverRunnerErr |> Error
    let private addError g f e = ((f |> g |> SolverRunnerErr) + e) |> Error

    let onSaveResult (proxy : SendMessageProxy) (r : ResultDataWithId) =
        printfn "onSaveResult: Sending results with resultDataId = %A." r.resultDataId

        {
            partitionerRecipient = proxy.partitionerId
            deliveryType = GuaranteedDelivery
            messageData = r |> SaveResultPrtMsg
        }.getMessageInfo()
        |> proxy.sendMessage
        |> Rop.bindError (addError OnSaveResultErr (SendResultMessageError (proxy.partitionerId.messagingClientId, r.resultDataId)))


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
            |> Rop.bindError (addError OnSaveChartsErr (SendChartMessageError (proxy.partitionerId.messagingClientId, c.resultDataId)))
        | NotGeneratedCharts ->
            printfn "onSaveCharts: No charts."
            Ok()


    let toDeliveryType progress =
        match progress with
        | NotStarted -> (GuaranteedDelivery, false)
        | InProgress _ -> (NonGuaranteedDelivery, false)
        | Completed _ -> (GuaranteedDelivery, true)
        | Failed _ -> (GuaranteedDelivery, true)
        | Cancelled _ -> (GuaranteedDelivery, true)
        | AllCoresBusy _ -> (GuaranteedDelivery, true)


    let onUpdateProgress (proxy : OnUpdateProgressProxy) (p : ProgressUpdateInfo) =
        //printfn "onUpdateProgress: runQueueId = %A, progress = %A." p.runQueueId p.progress
        let t, completed = toDeliveryType p.progress

        let r1 =
            {
                partitionerRecipient = proxy.sendMessageProxy.partitionerId
                deliveryType = t
                messageData = UpdateProgressPrtMsg p
            }.getMessageInfo()
            |> proxy.sendMessageProxy.sendMessage
            |> Rop.bindError (addError OnUpdateProgressErr (UnableToSendProgressMsgErr p.runQueueId))

        if completed
        then
            let r2 = proxy.tryDeleteWorkerNodeRunModelData p.runQueueId
            combineUnitResults r1 r2
        else r1


    let private tryLoadWorkerNodeSettings () = tryLoadWorkerNodeSettings None None
    let private name = WorkerNodeServiceName.netTcpServiceName.value.value |> MessagingClientName


//    type SendMessageProxy
//        with
//        static member create (w : WorkerNodeSettings) : SendMessageProxy =
//            failwith "SendMessageProxy.create is not yet implemented."
////            {
////                partitionerId = w.workerNodeInfo.partitionerId
////                sendMessage = 0
////            }


    type SolverRunnerProxy
        with
        static member create c logCrit (proxy : OnUpdateProgressProxy) =
            let checkCancellation q =
                match tryCheckCancellation c q with
                | Ok v -> v
                | Error _ -> None

            {
                updateProgress = onUpdateProgress proxy
                saveResult = onSaveResult proxy.sendMessageProxy
                saveCharts = onSaveCharts proxy.sendMessageProxy
                logCrit = logCrit
                checkCancellation = checkCancellation
            }

//    let tryRunSolverRunner (proxy : SolverProcessProxy) =
//        match proxy.tryLoadRunQueue (), tryLoadWorkerNodeSettings (), proxy.checkRunning() with
//        | Ok w, Some s, Ok() ->
//            let c = getWorkerNodeSvcConnectionString
//            match proxy.tryStartRunQueue () with
//            | Ok() ->
//                let solverProxy = SolverRunnerProxy.create c proxy
//                let solver = getSolverRunner solverProxy w
//                solver.runSolver()
//                Ok()
//            | Error e -> Error e
//        | Error e, _, _ -> Error e
//        | _, None, _ -> InvalidSettings "Unable to load settings." |> WrkSettingsErr |> WorkerNodeErr |> Error
//        | _, _, Error e -> Error e


//    type SolverProcess(proxy : SolverProcessProxy) =
//
//        member x.run() = tryRunSolverRunner proxy


//    let tryCreateSolver q : ClmResult<SolverProcess> =
//        let c = getWorkerNodeSvcConnectionString
//        let proxy = SolverProcessProxy.create c q
//        let solver = SolverProcess proxy
//        Ok solver

    // Send the message directly to local database.
    let private sendMessage c m i =
        createMessage messagingDataVersion m i
        |> saveMessage c


    let private tryStartRunQueue c q =
        let pid = Process.GetCurrentProcess().Id |> ProcessId
        tryStartRunQueue c q pid


    let runSolver (results : ParseResults<SolverRunnerArguments>) usage : int =
        let c = getWorkerNodeSvcConnectionString
        let logCrit = saveSolverRunnerErrFs name

        let exitWithLogCrit e x =
            SolverRunnerCriticalError.create e |> logCrit |> ignore
            x

        match results.TryGetResult RunQueue |> Option.bind (fun e -> e |> RunQueueId |> Some) with
        | Some q ->
            match tryLoadRunQueue c q, tryLoadWorkerNodeSettings(), checkRunning q with
            | Ok w, Some s, Ok() ->
                match tryStartRunQueue c q with
                | Ok() ->
                    let proxy =
                        {
                            tryDeleteWorkerNodeRunModelData = deleteRunQueue c

                            sendMessageProxy =
                                {
                                    partitionerId = s.workerNodeInfo.partitionerId
                                    sendMessage = sendMessage c s.workerNodeInfo.workerNodeId.messagingClientId
                                }
                        }

                    let solverProxy = SolverRunnerProxy.create c logCrit proxy
                    let solver = getSolverRunner solverProxy w
                    solver.runSolver()
                    CompletedSuccessfully
                | Error e -> exitWithLogCrit e UnknownException
            | Error e, _, _ -> exitWithLogCrit e DatabaseErrorOccurred
            | _, None, _ -> exitWithLogCrit "Unable to load WorkerNodeSettings." CriticalError
            | _, _, Error e -> exitWithLogCrit e CriticalError
        | None ->
            printfn $"runSolver: {usage}."
            InvalidCommandLineArgs


//        let g () = results.TryGetResult RunQueue |> Option.bind (fun e -> e |> RunQueueId |> tryLoadRunQueue c |> Some)
//
//        match g (), tryLoadWorkerNodeSettings () with
//        | Some (Ok q) ->
//            let proxy = SolverProcessProxy.create c q
//
//            match proxy.tryLoadRunQueue (), tryLoadWorkerNodeSettings (), proxy.checkRunning() with
//            | Ok w, Some s, Ok() ->
//                let c = getWorkerNodeSvcConnectionString
//                match proxy.tryStartRunQueue () with
//                | Ok() ->
//                    let solverProxy = SolverRunnerProxy.create c proxy
//                    let solver = getSolverRunner solverProxy w
//                    solver.runSolver()
//                    Ok()
//                | Error e -> Error e
//            | Error e, _, _ -> Error e
//            | _, None, _ -> InvalidSettings "Unable to load settings." |> WrkSettingsErr |> WorkerNodeErr |> Error
//            | _, _, Error e -> Error e
//
////            match tryCreateSolver q with
////            | Ok solver ->
////                solver.run()
////                CompletedSuccessfully
////            | Error e ->
////                printfn $"runSolver: Error: {e}."
////                DatabaseErrorOccurred
//        | _ ->
//            printfn $"runSolver: {usage}."
//            InvalidCommandLineArgs

