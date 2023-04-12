namespace SolverRunner

open ClmSys
open Microsoft.FSharp.Core

open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives
open Primitives.SolverRunnerErrors
open Softellect.Sys.Core
open Clm.ModelInit
open Clm.ModelParams
open Clm.ChartData
open OdeSolver.Solver
open Analytics.ChartExt
open Analytics.Visualization
open ContGenServiceInfo.ServiceInfo
open Clm.Distributions
open Clm.CalculationData
open ServiceProxy.SolverRunner
open System.IO
open ClmSys.ContGenPrimitives
open ClmSys.ClmErrors
open MessagingServiceInfo.ServiceInfo
open ClmSys.GeneralPrimitives
open System.Threading
open ClmSys.SolverRunnerPrimitives
open System
open ClmSys.TimerEvents
open Softellect.Sys.Logging

module SolverRunnerTasks =

    let getPlotDataInfo (df : ClmDefaultValueId) =
        let d = PlotDataInfo.defaultValue
        { d with resultInfo = { d.resultInfo with resultLocation = Path.Combine(d.resultInfo.resultLocation, df.ToString()) } }


    type AsyncChartDataUpdater = AsyncUpdater<ChartInitData, ChartSliceData, ChartData>


    type RunSolverData =
        {
            modelDataId : ModelDataId
            modelData : ModelData
            getInitValues : double -> double[]
            y0 : double
            useAbundant : bool
            chartInitData : ChartInitData
            chartDataUpdater : AsyncChartDataUpdater
            progressCallBack : RunQueueStatus option -> ProgressData -> unit
            updateChart : ChartSliceData -> unit
            getChartSliceData : double -> double[] -> EeData -> ChartSliceData
            noOfProgressPoints : int
            minUsefulEe : MinUsefulEe
            checkCancellation : RunQueueId -> CancellationType option
            checkFreq : TimeSpan
        }


        static member create (w : WorkerNodeRunModelData) (proxy : SolverUpdateProxy) =
            let modelDataParamsWithExtraData = w.modelData.modelData.getModelDataParamsWithExtraData()
            let modelDataId = modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.modelDataId
            let binaryInfo = modelDataParamsWithExtraData.binaryInfo
            let seed = modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.seedValue
            let rnd = RandomValueGetter.create (Some seed)
            let defaultValueId = w.modelData.modelData.modelDataParams.modelInfo.clmDefaultValueId
            let commandLineParams = w.runningProcessData.commandLineParams

            let logIfError v =
                match v with
                | Ok _ -> ()
                | Error e -> SolverRunnerCriticalError.create w.runningProcessData.runQueueId e |> proxy.logCrit |> ignore

            let updateProgress = proxy.updateProgress >> logIfError

            let r =
                {
                    modelDataId = modelDataId
                    defaultValueId = defaultValueId
                    runQueueId = w.runningProcessData.runQueueId
                    workerNodeId = w.runningProcessData.workerNodeId
                    commandLineParams = commandLineParams
                }

            let chartInitData =
                {
                    modelDataId = modelDataId
                    defaultValueId = defaultValueId
                    binaryInfo = binaryInfo
                    y0 = commandLineParams.y0
                    tEnd = commandLineParams.tEnd
                    description = w.modelData.modelData.modelDataParams.modelInfo.description
                }

            let chartDataUpdater = AsyncChartDataUpdater(ChartDataUpdater(), chartInitData)
            let getChartSliceData t x e = ChartSliceData.create binaryInfo t x e

            let createProgressUpdateInfo s p =
                {
                    runQueueId = r.runQueueId
                    updatedRunQueueStatus = s
                    progressData = p
                }

            {
                modelDataId = modelDataId
                modelData = w.modelData
                getInitValues = defaultInit rnd (ModelInitValuesParams.getDefaultValue modelDataParamsWithExtraData commandLineParams.useAbundant)
                y0 = double commandLineParams.y0
                useAbundant = commandLineParams.useAbundant
                chartInitData = chartInitData
                chartDataUpdater = chartDataUpdater
                updateChart = chartDataUpdater.addContent
                getChartSliceData = getChartSliceData
                progressCallBack = (fun s p -> createProgressUpdateInfo s p |> updateProgress)
                noOfProgressPoints = w.controlData.noOfProgressPoints
                minUsefulEe = w.controlData.minUsefulEe
                checkCancellation = proxy.checkCancellation
                checkFreq = TimeSpan.FromMinutes 60.0
            }


    type ChartData
        with
        member cd.toEeData() =
            {
                maxEe = cd.eeData.maxEe
                maxAverageEe = cd.eeData.maxAverageEe
                maxWeightedAverageAbsEe = cd.eeData.maxWeightedAverageAbsEe
                maxLastEe = cd.eeData.maxLastEe
            }


    let getNSolveParam (d : RunSolverData) (w : WorkerNodeRunModelData) : NSolveParam =
        let mutable lastCheck = DateTime.Now

        let checkCancellation =
            match w.controlData.earlyExitParamOpt |> Option.map EarlyExitInfo.getValue with
            | None -> d.checkCancellation
            | Some c ->
                let check r =
                    let fromLastCheck = DateTime.Now - lastCheck

                    if fromLastCheck > c.frequency.value
                    then
                        lastCheck <- DateTime.Now

                        match d.chartDataUpdater.getContent() |> c.earlyExitStrategy.exitEarly with
                        | true, e -> Some (CancelWithResults e)
                        | false, _ -> d.checkCancellation r

                    else d.checkCancellation r

                check

        {
            odeParams =
                {
                    startTime = 0.0
                    endTime = (double w.runningProcessData.commandLineParams.tEnd)
                    stepSize = 0.1
                    absoluteTolerance = w.controlData.absoluteTolerance
                    noOfOutputPoints = defaultNoOfOutputPoints
                    noOfProgressPoints = d.noOfProgressPoints
                    noOfChartDetailedPoints = Some 10
                }

            solverType = OdePack (Bdf, ChordWithDiagonalJacobian, UseNonNegative)
            modelDataId = d.modelDataId.value
            runQueueId = w.runningProcessData.runQueueId
            calculationData = d.modelData.modelData.modelBinaryData.calculationData
            initialValues = d.getInitValues d.y0
            progressCallBack = d.progressCallBack
            chartCallBack = d.updateChart
            getChartSliceData = d.getChartSliceData
            checkCancellation = checkCancellation
            checkFreq = d.checkFreq
        }


    type PlotResultsInfo =
        {
            runSolverData : RunSolverData
            runQueueId : RunQueueId
            chartData : ChartData
        }


    let plotAllResults t (i : PlotResultsInfo) =
        let plotAll () =
            let pdi = getPlotDataInfo i.runSolverData.modelData.modelData.modelDataParams.modelInfo.clmDefaultValueId
            let plotter = Plotter(pdi, i.chartData)

            {
                runQueueId = i.runQueueId
                defaultValueId = i.chartData.initData.defaultValueId
                charts =
                    [
                        plotter.getAminoAcids ()
                        plotter.getTotalSubst ()
                        plotter.getEnantiomericExcess ()
                    ]
            }
            |> GeneratedCharts

        printfn $"plotAllResults: i.chartData.maxEe = {i.chartData.eeData.maxEe}, i.runSolverData.minUsefulEe.value = {i.runSolverData.minUsefulEe.value}"
        match i.chartData.eeData.maxEe >= i.runSolverData.minUsefulEe.value, t with
        | true, _ -> plotAll ()
        | _, ForceChartGeneration -> plotAll ()
        | _ -> NotGeneratedCharts


    /// A function to test how to cancel hung up solvers.
    let private testCancellation (proxy : SolverRunnerProxy) (w : WorkerNodeRunModelData)  =
        let mutable counter = 0
        let mutable cancel = None

        while cancel = None do
            cancel <- proxy.solverUpdateProxy.checkCancellation w.runningProcessData.runQueueId
            printfn $"runSolver: runQueueId = %A{w.runningProcessData.runQueueId}, counter = %A{counter}, cancel = %A{cancel}"
            Thread.Sleep 10000
            counter <- counter + 1

        // kk:20200410 - Note that we have to resort to using exceptions for flow control here.
        // There seems to be no other easy and clean way. Revisit if that changes.
        // Follow the trail of that date stamp to find other related places.
        //
        // Note that we mimic the exception raised by the real solver when cancellation is requested.
        // See comments to the exception type below for reasoning.
        let m = $"testCancellation - Aborted at counter = %i{counter}." |> Some
        raise(ComputationAbortedException (ProgressData.defaultValue EeData.defaultValue, cancel |> Option.defaultValue (AbortCalculation None)))


    type SolverProxy =
        {
            runSolver : unit -> unit
            notifyOfCharts : ResultNotificationType -> UnitResult
            logIfFailed : UnitResult -> unit
            solverNotificationProxy : SolverNotificationProxy
        }


    type SolverRunner(proxy : SolverProxy, q : RunQueueId) =

        let logger = Logger.defaultValue

        let notifyOfCharts() =
            match proxy.solverNotificationProxy.checkNotificationRequest q with
            | Some t ->
                let r1 = proxy.notifyOfCharts t
                let r2 = proxy.solverNotificationProxy.clearNotificationRequest q
                combineUnitResults r1 r2
            | None -> Ok()

        let h = ClmEventHandler(ClmEventHandlerInfo.defaultValue logger notifyOfCharts "SolverRunner - notifyOfCharts")
        do h.start()


        member _.run() =
            printfn "SolverRunner.run was called."
            proxy.runSolver()
            printfn "SolverRunner.run - completed."


    let getProgress (w : WorkerNodeRunModelData) s p =
        {
            runQueueId = w.runningProcessData.runQueueId
            updatedRunQueueStatus = s
            progressData = p
        }


    let logIfFailed (proxy : SolverRunnerProxy) q errMessage result =
        match result with
        | Ok() -> ()
        | Error e -> SolverRunnerCriticalError.create q ($"{errMessage} + : + {e}") |> proxy.logCrit |> ignore


    let updateFinalProgress (proxy : SolverRunnerProxy) q errMessage = proxy.solverUpdateProxy.updateProgress >> (logIfFailed proxy q errMessage)


    let runSolver (proxy : SolverRunnerProxy) (w : WorkerNodeRunModelData) =
        let q = w.runningProcessData.runQueueId
        let logIfFailed = logIfFailed proxy q
        let updateFinalProgress = updateFinalProgress proxy q
        let runSolverData = RunSolverData.create w proxy.solverUpdateProxy
        let data = getNSolveParam runSolverData w
        let getChartData() = runSolverData.chartDataUpdater.getContent()

        let notifyOfCharts t =
            printfn $"notifyOfCharts: t = %A{t}"
            let chartData = getChartData()

            let chartResult =
                {
                    runSolverData = runSolverData
                    runQueueId = w.runningProcessData.runQueueId
                    chartData = chartData
                }
                |> plotAllResults t
                |> proxy.saveCharts

            printfn $"notifyOfResults completed with result: %A{chartResult}"
            chartResult

        let runSolverImpl() =
            try
                // Uncomment temporarily when you need to test cancellations.
                //testCancellation proxy w

                printfn $"runSolver: Calling nSolve for runQueueId = %A{w.runningProcessData.runQueueId}, modelDataId = %A{w.runningProcessData.modelDataId}..."
                let nSolveResult = nSolve data
                printfn $"runSolver: ...call to nSolve for runQueueId = %A{w.runningProcessData.runQueueId}, modelDataId = %A{w.runningProcessData.modelDataId} is completed."
                let result = notifyOfCharts RegularChartGeneration

                printfn $"runSolver: Notifying of completion for runQueueId = %A{w.runningProcessData.runQueueId}, modelDataId = %A{w.runningProcessData.modelDataId}..."
                let completedResult = (Some RunQueueStatus.CompletedRunQueue, nSolveResult.progressData) ||> getProgress w |> proxy.solverUpdateProxy.updateProgress
                combineUnitResults result completedResult |> (logIfFailed "getSolverRunner - runSolver failed on transmitting Completed")
                printfn $"runSolver: All completed for runQueueId = %A{w.runningProcessData.runQueueId}, modelDataId = %A{w.runningProcessData.modelDataId} is completed."
            with
            // See: https://stackoverflow.com/questions/49974736/how-to-declare-a-generic-exception-types-in-f
            // kk:20200410 - Note that we have to resort to using exceptions for flow control here.
            // There seems to be no other easy and clean way. Revisit if that changes.
            // Follow the trail of that date stamp to find other related places.
            | :? ComputationAbortedException<EeData> as ex ->
                printfn $"getSolverRunner - runSolver: Cancellation was requested for runQueueId = %A{w.runningProcessData.runQueueId}"

                match ex.cancellationType with
                | CancelWithResults e ->
                    notifyOfCharts ForceChartGeneration |> logIfFailed "Unable to send charts."
                    getProgress w (Some CompletedRunQueue) ex.progressData
                | AbortCalculation e ->
                    getProgress w (Some CancelledRunQueue) ex.progressData
                |> updateFinalProgress "getSolverRunner - ComputationAborted failed."
            | e ->
                let p = { (ProgressData.defaultValue EeData.defaultValue) with errorMessageOpt = $"{e}" |> ErrorMessage |> Some }
                getProgress w (Some FailedRunQueue) p |> (updateFinalProgress "getSolverRunner - Exception occurred.")

        let proxy =
            {
                runSolver = runSolverImpl
                notifyOfCharts = notifyOfCharts
                logIfFailed = logIfFailed "getSolverRunner - SolverRunner."
                solverNotificationProxy = proxy.solverNotificationProxy
            }

        SolverRunner(proxy, w.runningProcessData.runQueueId)
