namespace FredholmSolverTests

open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FredholmSolver.EeInfCharts
open Primitives.GeneralData
open System
open FluentAssertions.Execution
open FluentAssertions
open GenericOdeSolver.Primitives
open GenericOdeSolver.Solver
open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives
open FredholmSolver.Primitives
open FredholmSolver.Kernel
open FredholmSolver.EeInfModel
open FredholmSolver.EeInfChartData
open Primitives.SolverRunnerErrors
open Softellect.Sys.Core
open Softellect.Sys.Logging
open Xunit
open Xunit.Abstractions
open Plotly.NET
open Analytics.ChartExt
open Primitives.ChartPrimitives
open Primitives.WolframPrimitives


type CallBackResults =
    {
        progressCallBackCount : int
        completedCallBackCount : int
        cancelledCallBackCount : int
        abortedCallBackCount : int
        chartCallBackCount : int
    }

    static member defaultValue =
        {
            progressCallBackCount = 0
            completedCallBackCount = 0
            cancelledCallBackCount = 0
            abortedCallBackCount = 0
            chartCallBackCount = 0
        }


type OdeResultData =
    {
        modelData : EeInfModel
        callBackResults : CallBackResults
        result : CallBackData
        getData : double[] -> SubstanceData
        invStart : double
        chartData : ChartData
    }

    member r.toWolframData() =
        let d = $"{r.modelData.modelParams}".Replace("\n", " ").Replace("    ", " ").Replace("    ", " ").Replace("    ", " ").Replace("  ", " ").Replace("  ", " ")

        let descr = $"descr = \"{d}\";{Nl}{Nl}"

        let eta = r.modelData.kernelData.domain2D.eeDomain.points.value
        let zeta = r.modelData.kernelData.domain2D.infDomain.points.value
        let etaData = $"etaData = {(toWolframNotation eta)};{Nl}{Nl}"
        let zetaData = $"zetaData = {(toWolframNotation zeta)};{Nl}{Nl}"

        let substanceData = r.getData r.result.x
        let u = substanceData.protocell.toMatrix().value
        let uData = $"uData = {(toWolframNotation u)};{Nl}{Nl}"

        let ka = r.modelData.kernelData.ka.value.value
        let kaData = $"ka = {(toWolframNotation ka)};{Nl}{Nl}"

        let gamma = r.modelData.gamma.value.value
        let gammaData = $"gamma = {(toWolframNotation gamma)};{Nl}{Nl}"

        let w (e : ChartSliceData) =
            let g x = toWolframNotation x

            let a =
                [
                    $"{(g e.tChart)}"
                    $"{g e.statData.eeStatData.mean}"
                    $"{g e.statData.eeStatData.stdDev}"
                    $"{g e.statData.infStatData.mean}"
                    $"{g e.statData.infStatData.stdDev}"
                    $"{g e.statData.invariant}"
                    $"{g e.statData.total}"
                    $"{g e.statData.food}"
                    $"{g e.statData.waste}"
                ]
                |> joinStrings ", "

            $"{{ {a} }}"

        let chartTitles = $"{Nl}chartTitles = {{\"t\", \"eeMean\", \"eeStdDev\", \"infMean\", \"infStdDev\", \"invariant\", \"total\", \"food\", \"waste\"}};{Nl}"

        let chart =
            r.chartData.allChartData
            |> List.map w
            |> joinStrings $",{Nl}"

        let chartData = $"chartData = {{ {chart} }};{Nl}{Nl}"
        $"{descr}{etaData}{zetaData}{kaData}{gammaData}{uData}{chartTitles}{chartData}"


type OdeTests (output : ITestOutputHelper) =
    let writeLine s = output.WriteLine s
    let nullString : string = null
    let errInvTolerance = 1.0e-3

    let defaultOdeParams =
        {
            startTime = 0.0
            endTime = 10.0
            stepSize = 1.0e-3
            absoluteTolerance = AbsoluteTolerance.defaultValue
            solverType = OdePack (Bdf, ChordWithDiagonalJacobian, UseNonNegative)
            outputParams =
                {
                    noOfOutputPoints = 4_000
                    noOfProgressPoints = 100
                    noOfChartDetailedPoints = None
                }
        }

    let defaultNonlinearOdeParams = { defaultOdeParams with endTime = 200_000.0 }
    let defaultNonlinearOdeParams2 = { defaultOdeParams with endTime = 1_000_000.0 }
    // let defaultNonlinearOdeParams = { defaultOdeParams with endTime = 2000.0 }
    // let defaultNonlinearOdeParams2 = { defaultOdeParams with endTime = 10_000.0 }


    let outputMatrix (d : Domain2D) (m : Matrix<double>) =
        "000," + String.Join(",", d.infDomain.points.value)
        |> writeLine

        m.value
        |> Array.mapi (fun i e -> $"{d.eeDomain.points.value[i]}," + String.Join(",", e))
        |> Array.map (fun e -> (writeLine $"{e}"))
        |> ignore

        writeLine $"{Nl}===================================================================================={Nl}{Nl}"


    let outputEeInfModelData (data : EeInfModelParams) =
        writeLine $"data:{Nl}{data}"


    let outputResult md d (v : SubstanceData) r =
        let sd = calculateStat md v

        let s = $"t: {d.t}, call count: {d.progressData.callCount}, " +
                $"progress: {d.progressData.progress}, " +
                $"inv: {sd.invariant}, total: {sd.total}, " +
                $"mean: {(sd.eeStatData.mean, sd.infStatData.mean)}, stdDev: {(sd.eeStatData.stdDev, sd.infStatData.stdDev)}.{Nl}"

        writeLine s

        if r
        then
            writeLine "u data:"
            v.protocell.toMatrix() |> outputMatrix md.kernelData.domain2D


    let outputKa (md : EeInfModel) =
        writeLine "ka:"
        md.kernelData.ka.value |> outputMatrix md.kernelData.domain2D


    let outputGamma (md : EeInfModel) =
        writeLine "gamma:"
        md.gamma.value |> outputMatrix md.kernelData.domain2D


    let outputChart (cd : ChartData) =
        let f e =
            $"{e.tChart},{e.statData.eeStatData.mean},{e.statData.eeStatData.stdDev}," +
            $"{e.statData.infStatData.mean},{e.statData.infStatData.stdDev}," +
            $"{e.statData.invariant},{e.statData.total}," +
            $"{e.statData.food},{e.statData.waste}"

        let header = "t,eeMean,eeStdDev,infMean,infStdDev,invariant,total,food,waste"

        writeLine $"{Nl}{Nl}Chart data:"
        List.append [ header ] (cd.allChartData |> List.map f |> List.rev)
        |> List.map writeLine
        |> ignore

        let plotter = EeInfPlotter(cd)
        plotter.eeChart()
        plotter.uChart()


    let outputWolframData name (d : OdeResultData) =
        let wolframFileName = $@"C:\EeInf\{name}.m"

        let wolframData = d.toWolframData()
        File.WriteAllText(wolframFileName, wolframData)


    let nSolveParam p odeParams =
        let md = EeInfModel.create p
        let i = md.initialValues
        // let f t v = outputResult md t v
        let f t v = ()
        let v x = LinearData<SubstanceType, double>.create i.value.dataInfo x |> SubstanceData

        let n =
            {
                odeParams = odeParams
                runQueueId = Guid() |> RunQueueId
                initialValues = i.value.data
                derivative = md.derivativeCalculator f i.value.dataInfo
                callBackInfo =
                    {
                        checkFreq = TimeSpan.MaxValue
                        // needsCallBack = NeedsCallBack (fun c _ -> c, None)
                        progressCallBack = ProgressCallBack (fun _ _ -> ())
                        chartCallBack = ChartCallBack (fun _ _ -> ())
                        checkCancellation = CheckCancellation (fun _ -> None)
                    }

                started = DateTime.Now
                logger =
                    {
                        logCrit = fun e -> writeLine $"CRIT: {DateTime.Now}, {e}."
                        logError = fun e -> writeLine $"ERROR: {DateTime.Now}, {e}."
                        logWarn = fun e -> writeLine $"WARN: {DateTime.Now}, {e}."
                        logInfo = fun e -> writeLine $"INFO: {DateTime.Now}, {e}."
                        logDebug = fun e -> writeLine $"DEBUG: {DateTime.Now}, {e}."
                    }
            }

        n, v


    let callBack cr outputResult c (d : CallBackData) =
        match c with
        | RegularCallBack ->
            outputResult d
            { cr with progressCallBackCount = cr.progressCallBackCount + 1 }
        | FinalCallBack ct ->
            match ct with
            | CompletedCalculation ->
                outputResult d
                { cr with completedCallBackCount = cr.completedCallBackCount + 1 }
            | CancelledCalculation c ->
                match c with
                | CancelWithResults _  -> { cr with cancelledCallBackCount = cr.cancelledCallBackCount + 1 }
                | AbortCalculation _ -> { cr with abortedCallBackCount = cr.abortedCallBackCount + 1 }


    let chartCallBack cr _ _ = { cr with chartCallBackCount = cr.chartCallBackCount + 1 }


    let calLBackInfo n callBack charCallBack checkCancellation =
        {
            checkFreq = TimeSpan.FromMilliseconds(10.0)
            progressCallBack = ProgressCallBack callBack
            chartCallBack = ChartCallBack charCallBack
            checkCancellation = CheckCancellation checkCancellation
        }


    let odePackRun p cc odeParams =
        let mutable cr = CallBackResults.defaultValue
        outputEeInfModelData p

        let md = EeInfModel.create p
        let n, getData = nSolveParam p odeParams
        let outputResult b (d : CallBackData) = outputResult md d (getData d.x) b

        let callBack c d = cr <- callBack cr (outputResult false) c d

        let chartInitData =
            {
                y0 = 0.0M
                tEnd = 0.0M
                resultId = RunQueueId.getNewId()
                modelParams = md.modelParams
                domain2D = md.kernelData.domain2D
            }

        let chartDataUpdater = AsyncChartDataUpdater(ChartDataUpdater(), chartInitData)

        let getChartSliceData c (d : CallBackData) : ChartSliceData =
            let g() = getData d.x

            let substanceData =
                match c with
                | RegularCallBack -> None
                | FinalCallBack cct ->
                    match cct with
                    | CompletedCalculation -> g() |> Some
                    | CancelledCalculation ct ->
                        match ct with
                        | AbortCalculation _ -> None
                        | CancelWithResults _ -> g() |> Some
            {
                tChart = d.t
                progressChart = d.progressData
                statData = calculateStat md (getData d.x)
                substanceData = substanceData
            }

        let chartCallBack c d =
            getChartSliceData c d |> chartDataUpdater.addContent
            cr <- chartCallBack cr c d

        let c = calLBackInfo n callBack chartCallBack cc

        let nSolveParam = { n with callBackInfo = c }
        let invStart = getData nSolveParam.initialValues |> md.invariant
        outputKa md
        outputGamma md
        outputResult false { progressData = ProgressData.defaultValue; t = nSolveParam.odeParams.startTime; x = nSolveParam.initialValues }

        let result = nSolve nSolveParam
        outputResult true result
        let chartData = chartDataUpdater.getContent()
        chartData|> outputChart

        let odeResult =
            {
                modelData = md
                callBackResults = cr
                result = result
                getData = getData
                invStart = invStart
                chartData = chartData
            }

        odeResult


    let odePackShouldRun p odeParams shift name =
        let eeInfModelParams = { p with initParams = p.initParams.shifted shift; name = Some name }
        let r = odePackRun eeInfModelParams (fun _ -> None) odeParams

        outputWolframData name r

        let v = r.getData r.result.x
        let inv_tEnd = r.modelData.invariant v
        let diff = (inv_tEnd - r.invStart) / r.invStart

        use _ = new AssertionScope()
        r.result.Should().NotBeNull(nullString) |> ignore
        diff.Should().BeApproximately(0.0, errInvTolerance, nullString) |> ignore


    member private _.getCallerName([<CallerMemberName; Optional; DefaultParameterValue("")>] ?memberName: string) =
        let memberName = defaultArg memberName ""
        memberName


    [<Fact>]
    member t.odePack_ShouldRun () : unit = odePackShouldRun EeInfModelParams.defaultValue defaultOdeParams 0 (t.getCallerName())


    [<Fact>]
    member t.odePack_ShouldRunNonLinear () : unit = odePackShouldRun EeInfModelParams.defaultNonlinearValue defaultNonlinearOdeParams 1 (t.getCallerName())


    [<Fact>]
    member t.odePack_ShouldRunNonLinear2 () : unit = odePackShouldRun EeInfModelParams.defaultNonlinearValue2 defaultNonlinearOdeParams2 1 (t.getCallerName())


    [<Fact>]
    member _.odePack_ShouldCallBack () : unit =
        let r = odePackRun EeInfModelParams.defaultValue (fun _ -> None) defaultOdeParams

        use _ = new AssertionScope()
        r.result.Should().NotBeNull(nullString) |> ignore
        r.callBackResults.progressCallBackCount.Should().BeGreaterThan(0, nullString) |> ignore
        r.callBackResults.completedCallBackCount.Should().BeGreaterThan(0, nullString) |> ignore
        r.callBackResults.cancelledCallBackCount.Should().Be(0, nullString) |> ignore
        r.callBackResults.chartCallBackCount.Should().BeGreaterThan(0, nullString) |> ignore


    [<Fact>]
    member _.odePack_ShouldCancel () : unit =
        let mutable cr = CallBackResults.defaultValue

        let md = EeInfModelParams.defaultValue
        let model = EeInfModel.create md
        let r = odePackRun md (fun _ -> None) defaultOdeParams

        let n, getData = nSolveParam md defaultOdeParams
        let outputResult b d = outputResult model d (getData d.x) b

        let callBack c d = cr <- callBack cr (outputResult false) c d
        let chartCallBack c d = cr <- chartCallBack cr c d
        let checkCancellation _ = CancelWithResults None |> Some
        let c = calLBackInfo n callBack chartCallBack checkCancellation

        let nSolveParam = { n with callBackInfo = c }

        use _ = new AssertionScope()
        FluentActions.Invoking(fun () -> nSolve nSolveParam |> ignore).Should().Throw<ComputationAbortedException>(nullString) |> ignore
        cr.progressCallBackCount.Should().BeGreaterThan(0, nullString) |> ignore
        cr.completedCallBackCount.Should().Be(0, nullString) |> ignore
        cr.cancelledCallBackCount.Should().Be(1, nullString) |> ignore
        cr.abortedCallBackCount.Should().Be(0, nullString) |> ignore


    [<Fact>]
    member _.odePack_ShouldAbort () : unit =
        let mutable cr = CallBackResults.defaultValue

        let md = EeInfModelParams.defaultValue
        let model = EeInfModel.create md

        let n, getData = nSolveParam md defaultOdeParams
        let outputResult b d = outputResult model d (getData d.x) b

        let callBack c d = cr <- callBack cr (outputResult false) c d
        let chartCallBack c d = cr <- chartCallBack cr c d
        let checkCancellation _ = AbortCalculation None |> Some
        let c = calLBackInfo n callBack chartCallBack checkCancellation

        let nSolveParam = { n with callBackInfo = c }

        use _ = new AssertionScope()
        FluentActions.Invoking(fun () -> nSolve nSolveParam |> ignore).Should().Throw<ComputationAbortedException>(nullString) |> ignore
        cr.progressCallBackCount.Should().BeGreaterThan(0, nullString) |> ignore
        cr.completedCallBackCount.Should().Be(0, nullString) |> ignore
        cr.cancelledCallBackCount.Should().Be(0, nullString) |> ignore
        cr.abortedCallBackCount.Should().Be(1, nullString) |> ignore


    [<Fact>]
    member _.integrate_ShouldWork () : unit =

        let integrate2D (grid: double[][]) dx dy =
            let len1 = grid.Length - 1
            let len2 = grid[0].Length - 1
            let sum = grid |> Array.map (fun e -> e |> Array.sum) |> Array.sum
            let edgeSum1 = grid[0] |> Array.sum
            let edgeSum2 = grid[len1] |> Array.sum
            let edgeSum3 = grid |> Array.mapi (fun i _ -> grid[i][0]) |> Array.sum
            let edgeSum4 = grid |> Array.mapi (fun i _ -> grid[i][len2]) |> Array.sum
            let cornerSum = grid[0][0] + grid[len1][0] + grid[0][len2] + grid[len1][len2]
            let retVal = dx * dy * (4.0 * sum - 2.0 * (edgeSum1 + edgeSum2 + edgeSum3 + edgeSum4) + cornerSum) / 4.0
            retVal

        let xSteps = 101
        let ySteps = 101
        let dx = 1.0 / (double (xSteps - 1))
        let dy = 1.0 / (double (ySteps - 1))

        let grid = Array.init xSteps (fun x -> Array.init ySteps (fun y -> float x * dx * float y * dy))

        // grid
        // |> Array.map (fun e -> String.Join(",", e))
        // |> Array.map (fun e -> (writeLine $"{e}"))
        // |> ignore

        let result = integrate2D grid dx dy
        result.Should().BeApproximately(0.25, 0.001, nullString) |> ignore


    [<Fact>]
    member _.chart_ShouldWork() : unit =
        let xData = [0. .. 10.]
        let yData = [0. .. 10.]
        let description = toDescription "Title" "description"
        let chart = Chart.Point(xData, yData, UseDefaults = false)
        let htmlString = toEmbeddedHtmlWithDescription description chart
        let tempFilePath = @"C:\Temp\tempHtmlFile.html"
        File.WriteAllText(tempFilePath, htmlString)
