namespace SolverRunner

open Argu
open Clm.ModelParams
open ClmSys.ClmErrors
open ClmSys.ExitErrorCodes
open ClmSys.GeneralPrimitives
open ClmSys.SolverData
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
open ClmSys.SolverRunnerPrimitives
open ClmSys.SolverRunnerErrors
open DbData.MsgSvcDatabaseTypes
open System.Diagnostics
open ClmSys.VersionInfo

module SolverRunnerImplementation =

    let private toError g f = f |> g |> SolverRunnerErr |> Error
    let private addError g f e = ((f |> g |> SolverRunnerErr) + e) |> Error


    let onSaveResult (proxy : SendMessageProxy) (r : ResultDataWithId) =
        printfn $"onSaveResult: Sending results with resultDataId = %A{r.resultDataId}."

        {
            partitionerRecipient = proxy.partitionerId
            deliveryType = GuaranteedDelivery
            messageData = r |> SaveResultPrtMsg
        }.getMessageInfo()
        |> proxy.sendMessage
        |> Rop.bindError (addError OnSaveResultErr (SendResultMessageErr (proxy.partitionerId.messagingClientId, r.resultDataId)))


    let onSaveCharts (proxy : SendMessageProxy) (r : ChartGenerationResult) =
        match r with
        | GeneratedCharts c ->
            printfn $"onSaveCharts: Sending charts with resultDataId = %A{c.resultDataId}."

            {
                partitionerRecipient = proxy.partitionerId
                deliveryType = GuaranteedDelivery
                messageData = c |> SaveChartsPrtMsg
            }.getMessageInfo()
            |> proxy.sendMessage
            |> Rop.bindError (addError OnSaveChartsErr (SendChartMessageErr (proxy.partitionerId.messagingClientId, c.resultDataId)))
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
        printfn $"onUpdateProgress: runQueueId = %A{p.runQueueId}, progress = %A{p.progress}."
        let t, completed = toDeliveryType p.progress
        let r0 = proxy.tryUpdateProgress p.progress

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
                        updateTime = proxy.tryUpdateTime
                        checkCancellation = checkCancellation
                    }

                solverNotificationProxy =
                    {
                        checkNotificationRequest = checkNotification
                        clearNotificationRequest = clearNotification
                    }

                saveResult = onSaveResult proxy.sendMessageProxy
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


    let runSolverProcessImpl (results : ParseResults<SolverRunnerArguments>) usage : int =
        let c = getWorkerNodeSvcConnectionString
        let logCrit = saveSolverRunnerErrFs name

        match results.TryGetResult RunQueue |> Option.bind (fun e -> e |> RunQueueId |> Some) with
        | Some q ->
            let exitWithLogCrit e x =
                printfn $"runSolver: Error: {e}, exit code: {x}."
                SolverRunnerCriticalError.create q e |> logCrit |> ignore
                x

            match tryLoadRunQueue c q, tryLoadWorkerNodeSettings() with
            | Ok w, Some s ->
                match checkRunning q s.workerNodeInfo.noOfCores with
                | CanRun ->
                    match tryStartRunQueue c q with
                    | Ok() ->
                        let proxy =
                            {
                                tryDeleteWorkerNodeRunModelData = fun () -> deleteRunQueue c q
                                tryUpdateProgress = tryUpdateProgressRunQueue c q
                                tryUpdateTime = tryUpdateTime c q

                                sendMessageProxy =
                                    {
                                        partitionerId = s.workerNodeInfo.partitionerId
                                        sendMessage = sendMessage c s.workerNodeInfo.workerNodeId.messagingClientId
                                    }
                            }

                        let solverProxy = SolverRunnerProxy.create c logCrit proxy

                        printfn $"runSolver: Starting solver with runQueueId: {q}."
                        let solver = runSolver solverProxy w
                        // The call below does not return until the run is completed OR cancelled in some way.
                        solver.run()
                        CompletedSuccessfully
                    | Error e -> exitWithLogCrit e UnknownException
                | AlreadyRunning p -> exitWithLogCrit (AlreadyRunning p) UnknownException
                | TooManyRunning n -> exitWithLogCrit (TooManyRunning n) UnknownException
                | GetProcessesByNameExn e -> exitWithLogCrit e CriticalError
            | Error e, _ -> exitWithLogCrit e DatabaseErrorOccurred
            | _, None -> exitWithLogCrit "Unable to load WorkerNodeSettings." CriticalError
        | None ->
            printfn $"runSolver: {usage}."
            InvalidCommandLineArgs
