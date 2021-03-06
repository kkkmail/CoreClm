﻿namespace SolverRunner

open Microsoft.FSharp.Core

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
open ClmSys.SolverRunnerErrors
open ClmSys.GeneralPrimitives
open System.Threading
open ClmSys.SolverRunnerPrimitives
open System
open ClmSys.SolverData
open ClmSys.TimerEvents
open Softellect.Sys.Logging

module SolverRunnerTasks =

    let notify progressNotifier r t =
        progressNotifier
            {
                runQueueId = r
                progress = t
            }


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
            progressCallBack : (decimal -> UnitResult) option
            updateChart : ChartSliceData -> unit
            updateTime : TimeData -> UnitResult
            getChartSliceData : double -> double[] -> ChartSliceData
            noOfProgressPoints : int option
            minUsefulEe : MinUsefulEe
            checkCancellation : RunQueueId -> CancellationType option
            checkFreq : TimeSpan
            timeCheckFreq : TimeSpan
        }


        static member create (w : WorkerNodeRunModelData) (proxy : SolverUpdateProxy) pp =
            let modelDataParamsWithExtraData = w.modelData.modelData.getModelDataParamsWithExtraData()
            let modelDataId = modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.modelDataId
            let binaryInfo = modelDataParamsWithExtraData.binaryInfo
            let seed = modelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.seedValue
            let rnd = RandomValueGetter.create (Some seed)
            let defaultValueId = w.modelData.modelData.modelDataParams.modelInfo.clmDefaultValueId
            let commandLineParams = w.runningProcessData.commandLineParams

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
            let getChartSliceData t x = ChartSliceData.create binaryInfo t x

            {
                modelDataId = modelDataId
                modelData = w.modelData
                getInitValues = defaultInit rnd (ModelInitValuesParams.getDefaultValue modelDataParamsWithExtraData commandLineParams.useAbundant)
                y0 = double commandLineParams.y0
                useAbundant = commandLineParams.useAbundant
                chartInitData = chartInitData
                chartDataUpdater = chartDataUpdater
                updateChart = chartDataUpdater.addContent
                updateTime = proxy.updateTime
                getChartSliceData = getChartSliceData
                progressCallBack = (fun p -> notify proxy.updateProgress r.runQueueId (TaskProgress.create p)) |> Some
                noOfProgressPoints = pp
                minUsefulEe = w.minUsefulEe
                checkCancellation = proxy.checkCancellation
                checkFreq = TimeSpan.FromMinutes 60.0
                timeCheckFreq = TimeSpan.FromMinutes 120.0
            }


    type ChartData
        with
        member cd.toEeData() =
            {
                maxEe = cd.maxEe
                maxAverageEe = cd.maxAverageEe
                maxWeightedAverageAbsEe = cd.maxWeightedAverageAbsEe
                maxLastEe = cd.maxLastEe
            }


    let getNSolveParam (d : RunSolverData) (w : WorkerNodeRunModelData) =
        let mutable lastCheck = DateTime.Now

        let checkCancellation =
            match w.earlyExitOpt with
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
            solverType = AdamsFunctional
            modelDataId = d.modelDataId.value
            runQueueId = w.runningProcessData.runQueueId
            tStart = 0.0
            tEnd = (double w.runningProcessData.commandLineParams.tEnd)
            calculationData = d.modelData.modelData.modelBinaryData.calculationData
            initialValues = d.getInitValues d.y0
            progressCallBack = d.progressCallBack
            chartCallBack = Some d.updateChart
            timeCallBack = Some d.updateTime
            getChartSliceData = d.getChartSliceData
            noOfOutputPoints = None
            noOfProgressPoints = d.noOfProgressPoints
            checkCancellation = checkCancellation
            checkFreq = d.checkFreq
            timeCheckFreq = d.timeCheckFreq
        }


    let getResultAndChartData rdi w (d : RunSolverData) =
        let chartData = d.chartDataUpdater.getContent()

        let r =
            {
                resultDataId = rdi
                workerNodeId = w
                resultData =
                    {
                        modelDataId = d.modelDataId

                        y0 = decimal d.y0
                        tEnd = decimal chartData.tLast
                        useAbundant = d.useAbundant

                        maxEe = chartData.maxEe
                        maxAverageEe = chartData.maxAverageEe
                        maxWeightedAverageAbsEe = chartData.maxWeightedAverageAbsEe
                        maxLastEe = chartData.maxLastEe
                    }
            }

        (r, chartData)


    type PlotResultsInfo =
        {
            runSolverData : RunSolverData
            resultDataWithId : ResultDataWithId
            chartData : ChartData
        }


    let plotAllResults t (i : PlotResultsInfo) =
        let plotAll () =
            let pdi = getPlotDataInfo i.runSolverData.modelData.modelData.modelDataParams.modelInfo.clmDefaultValueId
            let plotter = Plotter(pdi, i.chartData)

            {
                resultDataId = i.resultDataWithId.resultDataId
                defaultValueId = i.chartData.initData.defaultValueId
                charts =
                    [
                        plotter.getAminoAcids ()
                        plotter.getTotalSubst ()
                        plotter.getEnantiomericExcess ()
                    ]
            }
            |> GeneratedCharts

        match i.resultDataWithId.resultData.maxEe >= i.runSolverData.minUsefulEe.value, t with
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
        raise(ComputationAbortedException (w.runningProcessData.runQueueId, cancel |> Option.defaultValue (AbortCalculation m)))


    type SolverProxy =
        {
            runSolver : unit -> unit
            notifyOfResults : ResultNotificationType -> UnitResult
            logIfFailed : UnitResult -> unit
            solverNotificationProxy : SolverNotificationProxy
        }


    type private SolverRunnerState =
        | NotRunningSolver
        | RunningSolver


    type SolverRunner(proxy : SolverProxy, q : RunQueueId) =

        let logger = Logger.defaultValue

        let notifyOfResults() =
            match proxy.solverNotificationProxy.checkNotificationRequest q with
            | Some t ->
                let r1 = proxy.notifyOfResults t
                let r2 = proxy.solverNotificationProxy.clearNotificationRequest q
                combineUnitResults r1 r2
            | None -> Ok()

        let h = ClmEventHandler(ClmEventHandlerInfo.defaultValue logger notifyOfResults "SolverRunner - notifyOfResults")
        do h.start()


        member _.run() =
            printfn "SolverRunner.run was called."
            proxy.runSolver()
            printfn "SolverRunner.run - completed."

//        member _.notifyOfResults t =
//            printfn "SolverRunner.notifyOfResults was called."
//            let result = proxy.notifyOfResults t
//            printfn "SolverRunner.notifyOfResults - completed."
//            result


    let runSolver (proxy : SolverRunnerProxy) (w : WorkerNodeRunModelData) =
        let q = w.runningProcessData.runQueueId

        let logIfFailed errMessage result =
            match result with
            | Ok() -> ()
            | Error e -> SolverRunnerCriticalError.create q ($"{errMessage} + : + {e}") |> proxy.logCrit |> ignore

        let updateFinalProgress errMessage = proxy.solverUpdateProxy.updateProgress >> (logIfFailed errMessage)
        let runSolverData = RunSolverData.create w proxy.solverUpdateProxy None
        let data = getNSolveParam runSolverData w
        let getResultAndChartData() = getResultAndChartData (w.runningProcessData.runQueueId.toResultDataId()) w.runningProcessData.workerNodeId runSolverData

        let notifyOfResults t =
            printfn $"notifyOfResults: t = %A{t}"
            let (r, chartData) = getResultAndChartData()
            let result = proxy.saveResult r

            let chartResult =
                {
                    runSolverData = runSolverData
                    resultDataWithId = r
                    chartData = chartData
                }
                |> plotAllResults t
                |> proxy.saveCharts

            let r = combineUnitResults result chartResult
            printfn $"notifyOfResults completed with result: %A{r}"
            r

        let getProgress p =
            {
                runQueueId = w.runningProcessData.runQueueId
                progress = p
            }

        let runSolverImpl() =
            try
                // Uncomment temporarily when you need to test cancellations.
                //testCancellation proxy w

                printfn $"runSolver: Calling nSolve for runQueueId = %A{w.runningProcessData.runQueueId}, modelDataId = %A{w.runningProcessData.modelDataId}..."
                nSolve data |> ignore
                printfn $"runSolver: ...call to nSolve for runQueueId = %A{w.runningProcessData.runQueueId}, modelDataId = %A{w.runningProcessData.modelDataId} is completed."
                let result = notifyOfResults RegularChartGeneration

                printfn $"runSolver: Notifying of completion for runQueueId = %A{w.runningProcessData.runQueueId}, modelDataId = %A{w.runningProcessData.modelDataId}..."
                let completedResult = (None, None) |> Completed |> getProgress |> proxy.solverUpdateProxy.updateProgress
                combineUnitResults result completedResult |> (logIfFailed "getSolverRunner - runSolver failed on transmitting Completed")
                printfn $"runSolver: All completed for runQueueId = %A{w.runningProcessData.runQueueId}, modelDataId = %A{w.runningProcessData.modelDataId} is completed."
            with
            // kk:20200410 - Note that we have to resort to using exceptions for flow control here.
            // There seems to be no other easy and clean way. Revisit if that changes.
            // Follow the trail of that date stamp to find other related places.
            | ComputationAbortedException (_, r) ->
                printfn $"getSolverRunner - runSolver: Cancellation was requested for runQueueId = %A{w.runningProcessData.runQueueId}"

                match r with
                | CancelWithResults s ->
                    notifyOfResults ForceChartGeneration |> logIfFailed "Unable to send results and charts."
                    ((getResultAndChartData() |> snd).progress |> Some, s) |> Completed |> getProgress
                | AbortCalculation s -> getProgress (Cancelled s)
                |> updateFinalProgress "getSolverRunner - ComputationAborted failed."
            | e -> e.ToString() |> ErrorMessage |> Failed |> getProgress |> (updateFinalProgress "getSolverRunner - Exception occurred.")

        let proxy =
            {
                runSolver = runSolverImpl
                notifyOfResults = notifyOfResults
                logIfFailed = logIfFailed "getSolverRunner - SolverRunner."
                solverNotificationProxy = proxy.solverNotificationProxy
            }

        SolverRunner(proxy, w.runningProcessData.runQueueId)

