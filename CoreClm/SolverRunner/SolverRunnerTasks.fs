namespace SolverRunner

open Microsoft.FSharp.Core
open ClmSys.GeneralData
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
open ClmSys.ClmErrors
open System

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
            updateChart : double -> double[] -> unit
            noOfProgressPoints : int option
            minUsefulEe : MinUsefulEe
            checkCancellation : RunQueueId -> CancellationType option
            checkFreq : TimeSpan
        }


        static member create (w : WorkerNodeRunModelData) n pp cc =
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
                }

            let chartDataUpdater = AsyncChartDataUpdater(ChartDataUpdater(), chartInitData)
            let updateChart = fun t x -> ChartSliceData.create binaryInfo t x |> chartDataUpdater.addContent

            {
                modelDataId = modelDataId
                modelData = w.modelData
                getInitValues = defaultInit rnd (ModelInitValuesParams.getDefaultValue modelDataParamsWithExtraData commandLineParams.useAbundant)
                y0 = double commandLineParams.y0
                useAbundant = commandLineParams.useAbundant
                chartInitData = chartInitData
                chartDataUpdater = chartDataUpdater
                updateChart = updateChart
                progressCallBack = (fun p -> notify n r.runQueueId (TaskProgress.create p)) |> Some
                noOfProgressPoints = pp
                minUsefulEe = w.minUsefulEe
                checkCancellation = cc
                checkFreq = TimeSpan.FromMilliseconds 1000.0
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
            modelDataId = d.modelDataId.value
            runQueueId = w.runningProcessData.runQueueId
            tStart = 0.0
            tEnd = (double w.runningProcessData.commandLineParams.tEnd)
            derivative = d.modelData.modelData.modelBinaryData.calculationData.getDerivative
            initialValues = d.getInitValues d.y0
            progressCallBack = d.progressCallBack
            chartCallBack = Some d.updateChart
            getEeData = (fun () -> d.chartDataUpdater.getContent().toEeData()) |> Some
            noOfOutputPoints = None
            noOfProgressPoints = d.noOfProgressPoints
            checkCancellation = checkCancellation
            checkFreq = d.checkFreq
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
            let plotter = new Plotter(pdi, i.chartData)

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
            cancel <- proxy.checkCancellation w.runningProcessData.runQueueId
            printfn "runSolver: runQueueId = %A, counter = %A, cancel = %A" w.runningProcessData.runQueueId counter cancel
            Thread.Sleep 10000
            counter <- counter + 1

        // kk:20200410 - Note that we have to resort to using exceptions for flow control here.
        // There seems to be no other easy and clean way. Revisit if that changes.
        // Follow the trail of that date stamp to find other related places.
        //
        // Note that we mimic the exception raised by the real solver when cancellation is requested.
        // See comments to the exception type below for reasoning.
        let m = sprintf "testCancellation - Aborted at counter = %i." counter |> Some
        raise(ComputationAbortedException (w.runningProcessData.runQueueId, cancel |> Option.defaultValue (AbortCalculation m)))


    type SolverProxy =
        {
            runSolver : unit -> unit
            notifyOfResults : ResultNotificationType -> UnitResult
            logIfFailed : UnitResult -> unit
        }


    type private SolverRunnerState =
        | NotRunningSolver
        | RunningSolver


    type SolverRunnerMessage =
        | RunSolver
        | NotifyOfResults of ResultNotificationType


    type SolverRunner(proxy : SolverProxy) =
        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | RunSolver ->
                                printfn "SolverRunner.RunSolver"
                                match s with
                                | NotRunningSolver ->
                                    let m = async { proxy.runSolver() }
                                    Async.Start m
                                    printfn "SolverRunner.RunSolver - started."
                                | RunningSolver -> ignore()
                                return! RunningSolver |> loop
                            | NotifyOfResults t ->
                                printfn "SolverRunner.NotifyOfResults: %A" t
                                proxy.notifyOfResults t |> proxy.logIfFailed
                                printfn "SolverRunner.NotifyOfResults - completed."
                                return! s |> loop
                        }

                NotRunningSolver |> loop
                )

        member _.runSolver() = messageLoop.Post RunSolver

        member _.notifyOfResults t =
            printfn "SolverRunner.notifyOfResults was called."
            NotifyOfResults t |> messageLoop.Post
            printfn "SolverRunner.notifyOfResults - completed."
            Ok()


    /// Uncomment printfn below in case of severe issues.
    /// Then run ContGenService and WorkerNodeService as EXE with redirect into dump files.
    let getSolverRunner (proxy : SolverRunnerProxy) (w : WorkerNodeRunModelData) =
        let logIfFailed errMessage result =
            match result with
            | Ok() -> ignore()
            | Error e -> SolverRunnerCriticalError.fromErrMessage (errMessage + ":" + e.ToString()) |> proxy.logCrit |> ignore

        let updateFinalProgress errMessage = proxy.updateProgress >> (logIfFailed errMessage)
        let runSolverData = RunSolverData.create w proxy.updateProgress None proxy.checkCancellation
        let data = getNSolveParam runSolverData w
        let getResultAndChartData() = getResultAndChartData (w.runningProcessData.runQueueId.toResultDataId()) w.runningProcessData.workerNodeId runSolverData

        let notifyOfResults t =
            printfn "notifyOfResults: t = %A" t
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
            printfn "notifyOfResults completed with result: %A" r
            r

        let getProgress p =
            {
                runQueueId = w.runningProcessData.runQueueId
                progress = p
            }

        let runSolver() =
            try
                // Uncomment temporarily when you need to test cancellations.
                //testCancellation proxy w

                printfn "runSolver: Calling nSolve for runQueueId = %A, modelDataId = %A..." w.runningProcessData.runQueueId w.runningProcessData.modelDataId
                nSolve data |> ignore
                printfn "runSolver: ...call to nSolve for runQueueId = %A, modelDataId = %A is completed." w.runningProcessData.runQueueId w.runningProcessData.modelDataId
                let result = notifyOfResults RegularChartGeneration

                printfn "runSolver: Notifying of completion for runQueueId = %A, modelDataId = %A..." w.runningProcessData.runQueueId w.runningProcessData.modelDataId
                let completedResult = (None, None) |> Completed |> getProgress |> proxy.updateProgress
                combineUnitResults result completedResult |> (logIfFailed "getSolverRunner - runSolver failed on transmitting Completed")
                printfn "runSolver: All completed for runQueueId = %A, modelDataId = %A is completed." w.runningProcessData.runQueueId w.runningProcessData.modelDataId
            with
            // kk:20200410 - Note that we have to resort to using exceptions for flow control here.
            // There seems to be no other easy and clean way. Revisit if that changes.
            // Follow the trail of that date stamp to find other related places.
            | ComputationAbortedException (_, r) ->
                printfn "getSolverRunner - runSolver: Cancellation was requested for runQueueId = %A" w.runningProcessData.runQueueId

                match r with
                | CancelWithResults s ->
                    notifyOfResults ForceChartGeneration |> logIfFailed "Unable to send results and charts."
                    ((getResultAndChartData() |> snd).progress |> Some, s) |> Completed |> getProgress
                | AbortCalculation s -> getProgress (Cancelled s)
                |> updateFinalProgress "getSolverRunner - ComputationAborted failed."
            | e -> e.ToString() |> ErrorMessage |> Failed |> getProgress |> (updateFinalProgress "getSolverRunner - Exception occurred.")

        {
            runSolver = runSolver
            notifyOfResults = notifyOfResults
            logIfFailed = logIfFailed "getSolverRunner - SolverRunner."
        }
        |> SolverRunner
