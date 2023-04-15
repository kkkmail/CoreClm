namespace SolverRunner

open Argu
open ClmSys.ClmErrors
open ClmSys.ExitErrorCodes
open ClmSys.GeneralPrimitives
open ClmSys.SolverRunnerPrimitives
open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives
open ServiceProxy.SolverProcessProxy
open SolverRunner.SolverRunnerCommandLine
open NoSql.FileSystemTypes
open DbData.Configuration
open DbData.WorkerNodeDatabaseTypes
open Softellect.Sys
open Softellect.Messaging.Primitives
open Softellect.Messaging.Client
open ClmSys.WorkerNodeData
open ContGenServiceInfo.ServiceInfo
open WorkerNodeServiceInfo.ServiceInfo
open MessagingServiceInfo.ServiceInfo
open ClmSys.ContGenPrimitives
open ClmSys.WorkerNodePrimitives
open ServiceProxy.SolverRunner
open SolverRunner.SolverRunnerTasks
open DbData.MsgSvcDatabaseTypes
open System.Diagnostics
open ClmSys.VersionInfo
open Primitives.SolverRunnerErrors

module SolverRunnerImplementation =

    /// TODO kk:20210522 - Shall it be moved into some setting?
    /// Extra solver's "overhead" allowed when running SolverRunner by hands.
    /// This is needed when two versions share the same machine and one version has some stuck
    /// work, which needs to be started.
    ///
    /// WorkNodeService does not use that overhead and so it will not start more than allowed.
    [<Literal>]
    let AllowedOverhead = 0.20

    let private toError g f = f |> g |> SolverRunnerErr |> Error
    let private addError g f e = ((f |> g |> SolverRunnerErr) + e) |> Error


    let onSaveCharts (proxy : SendMessageProxy) (r : ChartGenerationResult) =
        match r with
        | GeneratedCharts c ->
            printfn $"onSaveCharts: Sending charts with runQueueId = %A{c.runQueueId}."

            {
                partitionerRecipient = proxy.partitionerId
                deliveryType = GuaranteedDelivery
                messageData = c |> SaveChartsPrtMsg
            }.getMessageInfo()
            |> proxy.sendMessage
            |> Rop.bindError (addError OnSaveChartsErr (SendChartMessageErr (proxy.partitionerId.messagingClientId, c.runQueueId)))
        | NotGeneratedCharts ->
            printfn "onSaveCharts: No charts."
            Ok()


    let toDeliveryType (p : ProgressUpdateInfo) =
        match p.updatedRunQueueStatus with
        | Some s ->
            match s with
            | NotStartedRunQueue -> (GuaranteedDelivery, false)
            | InactiveRunQueue -> (GuaranteedDelivery, false)
            | RunRequestedRunQueue -> (GuaranteedDelivery, false)
            | InProgressRunQueue -> (GuaranteedDelivery, false)
            | CompletedRunQueue -> (GuaranteedDelivery, true)
            | FailedRunQueue -> (GuaranteedDelivery, true)
            | CancelRequestedRunQueue -> (GuaranteedDelivery, false)
            | CancelledRunQueue -> (GuaranteedDelivery, true)

        | None -> (NonGuaranteedDelivery, false)


    let onUpdateProgress (proxy : OnUpdateProgressProxy) (p : ProgressUpdateInfo) =
        printfn $"onUpdateProgress: runQueueId = %A{p.runQueueId}, progress = %A{p.progressData}."
        let t, completed = toDeliveryType p
        let r0 = proxy.tryUpdateProgressData p.progressData

        let r1 =
            {
                partitionerRecipient = proxy.sendMessageProxy.partitionerId
                deliveryType = t
                messageData = UpdateProgressPrtMsg p
            }.getMessageInfo()
            |> proxy.sendMessageProxy.sendMessage
            |> Rop.bindError (addError OnUpdateProgressErr (UnableToSendProgressMsgErr p.runQueueId))

        let result =
            if completed
            then
                let r2 = proxy.tryDeleteWorkerNodeRunModelData()
//                let r2 = Ok()
                foldUnitResults [ r0; r1; r2 ]
            else foldUnitResults [ r0; r1 ]

        printfn $"    onUpdateProgress: runQueueId = %A{p.runQueueId}, result = %A{result}."
        result


    let private tryLoadWorkerNodeSettings () = tryLoadWorkerNodeSettings None None
    let private name = WorkerNodeServiceName.netTcpServiceName.value.value |> MessagingClientName


    type SolverRunnerProxy
        with
        static member create c logCrit (proxy : OnUpdateProgressProxy) =
            let checkCancellation q = tryCheckCancellation c q |> Rop.toOption |> Option.bind id
            let checkNotification q = tryCheckNotification c q |> Rop.toOption |> Option.bind id
            let clearNotification q = tryClearNotification c q

            {
                solverUpdateProxy =
                    {
                        updateProgress = onUpdateProgress proxy
                        checkCancellation = checkCancellation
                        logCrit = logCrit
                    }

                solverNotificationProxy =
                    {
                        checkNotificationRequest = checkNotification
                        clearNotificationRequest = clearNotification
                    }

                saveCharts = onSaveCharts proxy.sendMessageProxy
                logCrit = logCrit
            }


    // Send the message directly to local database.
    let private sendMessage c m i =
        createMessage messagingDataVersion m i
        |> saveMessage c


    let private tryStartRunQueue c q =
        let pid = Process.GetCurrentProcess().Id |> ProcessId
        tryStartRunQueue c q pid


    let getAllowedSolvers i =
        let noOfCores = i.noOfCores
        max ((float noOfCores) * (1.0 + AllowedOverhead) |> int) (noOfCores + 1)


    let runSolverProcessImpl (results : ParseResults<SolverRunnerArguments>) usage : int =
        let c = getWorkerNodeSvcConnectionString
        let logCrit = saveSolverRunnerErrFs name

        match results.TryGetResult RunQueue |> Option.bind (fun e -> e |> RunQueueId |> Some) with
        | Some q ->
            let exitWithLogCrit e x =
                printfn $"runSolver: ERROR: {e}, exit code: {x}."
                SolverRunnerCriticalError.create q e |> logCrit |> ignore
                x

            match tryLoadRunQueue c q, tryLoadWorkerNodeSettings() with
            | Ok (w, st), Some s ->
                let allowedSolvers =
                    match results.TryGetResult ForceRun |> Option.defaultValue false with
                    | false -> getAllowedSolvers s.workerNodeInfo |> Some
                    | true -> None

                match checkRunning allowedSolvers q with
                | CanRun ->
                    let proxy =
                        {
                            tryDeleteWorkerNodeRunModelData = fun () -> deleteRunQueue c q
                            tryUpdateProgressData = tryUpdateProgress c q

                            sendMessageProxy =
                                {
                                    partitionerId = s.workerNodeInfo.partitionerId
                                    sendMessage = sendMessage c s.workerNodeInfo.workerNodeId.messagingClientId
                                }
                        }

                    let solverProxy = SolverRunnerProxy.create c logCrit proxy

                    match st with
                    | NotStartedRunQueue | InProgressRunQueue ->
                        match tryStartRunQueue c q with
                        | Ok() ->
                            printfn $"runSolver: Starting solver with runQueueId: {q}."
                            let solver = runSolver solverProxy w
                            // The call below does not return until the run is completed OR cancelled in some way.
                            solver.run()
                            printfn "runSolver: Call to solver.run() completed."
                            CompletedSuccessfully
                        | Error e -> exitWithLogCrit e UnknownException
                    | CancelRequestedRunQueue ->
                        // If we got here that means that the solver was terminated before it had a chance to process cancellation.
                        // At this point we have no choice but abort the calculation because there is no data available to continue.
                        let errMessage = "The solver was terminated before processing cancellation. Aborting."
                        let p = { (ClmProgressData.defaultValue ClmProgressAdditionalData.defaultValue) with errorMessageOpt = errMessage |> ErrorMessage |> Some }
                        getProgress w (Some FailedRunQueue) p |> (updateFinalProgress solverProxy q errMessage)
                        exitWithLogCrit errMessage NotProcessedCancellation
                    | _ -> exitWithLogCrit ($"Invalid run queue status: {st}") InvalidRunQueueStatus
                | AlreadyRunning p -> exitWithLogCrit (AlreadyRunning p) SolverAlreadyRunning
                | TooManyRunning n -> exitWithLogCrit (TooManyRunning n) TooManySolversRunning
                | GetProcessesByNameExn e -> exitWithLogCrit e CriticalError
            | Error e, _ -> exitWithLogCrit e DatabaseErrorOccurred
            | _, None -> exitWithLogCrit "Unable to load WorkerNodeSettings." CriticalError
        | None ->
            printfn $"runSolver: {usage}."
            InvalidCommandLineArgs
