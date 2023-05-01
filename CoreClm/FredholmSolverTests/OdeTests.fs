namespace FredholmSolverTests

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
    }


/// TODO kk:20230413 - Consolidate data creation into some common place.
type OdeTests (output : ITestOutputHelper) =
    let writeLine s = output.WriteLine s
    let nullString : string = null
    let errInvTolerance = 1.0e-3
    let domain2D (data : KernelData) = Domain2D.create data.domainIntervals.value data.infMaxValue.value


    /// Creates a "delta" function centered near (0, 0) in the domain,
    /// which is a middle point in ee domain and 0-th point in inf domain.
    let getDeltaU (data : KernelData) eps eeShift =
        let domain = domain2D data
            // if data.domainIntervals.value % 2 = 0
            // then domain2D data
            // else failwith "data.noOfIntervals must be odd for this method to work."

        let g i j =
            match data.domainIntervals.value % 2 = 0 with
            | true -> if ((i + eeShift) * 2 = data.domainIntervals.value) && (j = 0) then 1.0 else 0.0
            | false ->
                if (((i + eeShift) * 2 = data.domainIntervals.value - 1) || ((i + eeShift) * 2 = data.domainIntervals.value + 1)) && (j = 0) then 1.0 else 0.0

        let v = domain.eeDomain.points.value |> Array.mapi (fun i _ -> domain.infDomain.points.value |> Array.mapi (fun j _ -> g i j)) |> Matrix
        let norm = domain.integrateValues v
        (eps / norm) * v


    let initialValues (md : EeInfModelData) eeShift =
        let f = FoodData (md.initParams.total - (double md.modelParams.numberOfMolecules.value) * md.initParams.eps)
        let w = WasteData 0.0
        let u = getDeltaU md.modelParams.kernelData md.initParams.eps eeShift |> ProtocellData
        let sd = SubstanceData.create f w u
        sd


    let defaultOdeParams =
        {
            startTime = 0.0
            endTime = 10.0
            stepSize = 1.0e-3
            absoluteTolerance = AbsoluteTolerance.defaultValue
            solverType = OdePack (Bdf, ChordWithDiagonalJacobian, UseNonNegative)
            outputParams =
                {
                    noOfOutputPoints = 1_000
                    noOfProgressPoints = 100
                    noOfChartDetailedPoints = None
                }
        }

    let defaultNonlinearOdeParams =
        { defaultOdeParams with endTime = 200_000.0 }

    let defaultNonlinearOdeParams2 =
        { defaultOdeParams with endTime = 400_000.0 }

    let outputMatrix (d : Domain2D) (m : Matrix<double>) =
        "000," + String.Join(",", d.infDomain.points.value)
        |> writeLine

        m.value
        |> Array.mapi (fun i e -> $"{d.eeDomain.points.value[i]}," + String.Join(",", e))
        |> Array.map (fun e -> (writeLine $"{e}"))
        |> ignore

        writeLine $"{Nl}===================================================================================={Nl}{Nl}"


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
            v.protocell.toMatrix() |> outputMatrix md.kernel.domain2D

    let outputKa (md : EeInfModel) =
        writeLine "ka:"
        md.kernel.kaValue.value |> outputMatrix md.kernel.domain2D


    let outputGamma (md : EeInfModel) =
        writeLine "gamma:"
        md.gamma.value |> outputMatrix md.kernel.domain2D


    let outputChart (cd : ChartData) =
        let f e =
            $"{e.tChart},{e.statData.eeStatData.mean},{e.statData.infStatData.mean}," +
            $"{e.statData.eeStatData.stdDev},{e.statData.infStatData.stdDev}," +
            $"{e.statData.total},{e.statData.invariant}" +
            $"{e.statData.food},{e.statData.waste}"

        let header = "t,eeMean,infMean,eeStdDev,infStdDev,total,invariant,food,waste"

        writeLine $"{Nl}{Nl}Chart data:"
        List.append [ header ] (cd.allChartData |> List.map f |> List.rev)
        |> List.map writeLine
        |> ignore


    let nSolveParam (data : EeInfModelData) odeParams eeShift =
        let md = EeInfModel.create data.modelParams
        let i = initialValues data eeShift
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


    let odePackRun (data : EeInfModelData) cc odeParams eeShift =
        let mutable cr = CallBackResults.defaultValue

        let md = EeInfModel.create data.modelParams
        let n, getData = nSolveParam data odeParams eeShift
        let outputResult b (d : CallBackData) = outputResult md d (getData d.x) b

        let callBack c d = cr <- callBack cr (outputResult false) c d
        let chartInitData = { y0 = 0.0M; tEnd = 0.0M }
        let chartDataUpdater = AsyncChartDataUpdater(ChartDataUpdater(), chartInitData)

        let getChartSliceData (d : CallBackData) : ChartSliceData =
            {
                tChart = d.t
                progressChart = d.progressData
                statData = calculateStat md (getData d.x)
            }

        let chartCallBack c d =
            getChartSliceData d |> chartDataUpdater.addContent
            cr <- chartCallBack cr c d

        let c = calLBackInfo n callBack chartCallBack cc

        let nSolveParam = { n with callBackInfo = c }
        let invStart = getData nSolveParam.initialValues |> md.invariant
        outputKa md
        outputGamma md
        outputResult false { progressData = ProgressData.defaultValue; t = nSolveParam.odeParams.startTime; x = nSolveParam.initialValues }

        let result = nSolve nSolveParam
        outputResult true result
        chartDataUpdater.getContent() |> outputChart

        {
            modelData = md
            callBackResults = cr
            result = result
            getData = getData
            invStart = invStart
        }


    let odePackShouldRun eeInfModelData odeParams eeShift =
        let r = odePackRun eeInfModelData (fun _ -> None) odeParams eeShift

        let v = r.getData r.result.x
        let inv_tEnd = r.modelData.invariant v
        let diff = (inv_tEnd - r.invStart) / r.invStart

        use _ = new AssertionScope()
        r.result.Should().NotBeNull(nullString) |> ignore
        diff.Should().BeApproximately(0.0, errInvTolerance, nullString) |> ignore


    [<Fact>]
    member _.odePack_ShouldRun () : unit = odePackShouldRun EeInfModelData.defaultValue defaultOdeParams 0


    [<Fact>]
    member _.odePack_ShouldRunNonLinear () : unit = odePackShouldRun EeInfModelData.defaultNonlinearValue defaultNonlinearOdeParams 1


    [<Fact>]
    member _.odePack_ShouldRunNonLinear2 () : unit = odePackShouldRun EeInfModelData.defaultNonlinearValue2 defaultNonlinearOdeParams2 1


    [<Fact>]
    member _.odePack_ShouldCallBack () : unit =
        let r = odePackRun EeInfModelData.defaultValue (fun _ -> None) defaultOdeParams 0

        use _ = new AssertionScope()
        r.result.Should().NotBeNull(nullString) |> ignore
        r.callBackResults.progressCallBackCount.Should().BeGreaterThan(0, nullString) |> ignore
        r.callBackResults.completedCallBackCount.Should().BeGreaterThan(0, nullString) |> ignore
        r.callBackResults.cancelledCallBackCount.Should().Be(0, nullString) |> ignore
        r.callBackResults.chartCallBackCount.Should().BeGreaterThan(0, nullString) |> ignore


    [<Fact>]
    member _.odePack_ShouldCancel () : unit =
        let mutable cr = CallBackResults.defaultValue

        let md = EeInfModelData.defaultValue
        let model = EeInfModel.create md.modelParams
        let r = odePackRun md (fun _ -> None) defaultOdeParams

        let n, getData = nSolveParam md defaultOdeParams 0
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

        let md = EeInfModelData.defaultValue
        let model = EeInfModel.create md.modelParams

        let n, getData = nSolveParam md defaultOdeParams 0
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
