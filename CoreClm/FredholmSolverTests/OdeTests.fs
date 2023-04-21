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
open Primitives.SolverRunnerErrors
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


type EeInfInitParams =
    {
        eps : double
        total : double
    }


type EeInfModelData =
    {
        modelParams : EeInfModelParams
        initParams : EeInfInitParams
    }

    static member defaultValue =
        {
            modelParams = EeInfModelParams.defaultValue
            initParams =
                {
                    eps = 1.0e-2
                    total = 10.0
                }
        }

    static member defaultNonlinearValue =
        let data = EeInfModelData.defaultValue
        let domain2D = Domain2D.create data.modelParams.kernelData.domainIntervals.value data.modelParams.kernelData.infMaxValue.value
        { EeInfModelData.defaultValue with modelParams = EeInfModelParams.defaultNonlinearValue domain2D }


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
    let getDeltaU (data : KernelData) eps =
        let domain =
            if data.domainIntervals.value % 2 = 1
            then domain2D data
            else failwith "data.noOfIntervals must be odd for this method to work."

        let g i j = if (i * 2 + 1 = data.domainIntervals.value) && (j = 0) then 1.0 else 0.0
        let v = domain.eeDomain.midPoints.value |> Array.mapi (fun i _ -> domain.infDomain.midPoints.value |> Array.mapi (fun j _ -> g i j)) |> Matrix
        let norm = domain.integrateValues v
        (eps / norm) * v


    let initialValues (md : EeInfModelData) = // (data : KernelData) eid =
        let f = FoodData (md.initParams.total - (double md.modelParams.numberOfMolecules.value) * md.initParams.eps)
        let w = WasteData 0.0
        let u = getDeltaU md.modelParams.kernelData md.initParams.eps |> ProtocellData
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
                    noOfOutputPoints = 100
                    noOfProgressPoints = 100
                    noOfChartDetailedPoints = None
                }
        }

    let defaultNonlinearOdeParams =
        { defaultOdeParams with endTime = 1_000_000.0 }


    let outputResult md d (v : SubstanceData) r =
        let u = v.protocell
        let total = md.kernel.domain2D.integrateValues u
        let inv = md.invariant v
        let m = md.kernel.domain2D.mean u
        let s = md.kernel.domain2D.stdDev u
        let um = u.toMatrix()
        writeLine $"t: {d.t}, call count: {d.progressData.callCount}, progress: {d.progressData.progress}, inv: {inv}, total: {total}, mean: {m}, stdDev: {s}."

        if r
        then
            um.value
            |> Array.map (fun e -> String.Join(",", e))
            |> Array.map (fun e -> (writeLine $"{e}"))
            |> ignore


    let nSolveParam (data : EeInfModelData) odeParams =
        let md = EeInfModel.create data.modelParams
        let i = initialValues data
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
            // needsCallBack = n.odeParams.outputParams.needsCallBack n
            progressCallBack = ProgressCallBack callBack
            chartCallBack = ChartCallBack charCallBack
            checkCancellation = CheckCancellation checkCancellation
        }


    let odePackRun (data : EeInfModelData) cc odeParams =
        let mutable cr = CallBackResults.defaultValue

        let md = EeInfModel.create data.modelParams
        let n, getData = nSolveParam data odeParams
        let outputResult b (d : CallBackData) = outputResult md d (getData d.x) b

        let callBack c d = cr <- callBack cr (outputResult false) c d
        let chartCallBack c d = cr <- chartCallBack cr c d
        let c = calLBackInfo n callBack chartCallBack cc

        let nSolveParam = { n with callBackInfo = c }
        let invStart = getData nSolveParam.initialValues |> md.invariant
        outputResult true { progressData = ProgressData.defaultValue; t = nSolveParam.odeParams.startTime; x = nSolveParam.initialValues }

        let result = nSolve nSolveParam
        outputResult true result

        {
            modelData = md
            callBackResults = cr
            result = result
            getData = getData
            invStart = invStart
        }


    [<Fact>]
    member _.odePack_ShouldRun () : unit =
        let r = odePackRun EeInfModelData.defaultValue (fun _ -> None) defaultOdeParams

        let v = r.getData r.result.x
        let inv_tEnd = r.modelData.invariant v
        let diff = (inv_tEnd - r.invStart) / r.invStart

        use _ = new AssertionScope()
        r.result.Should().NotBeNull(nullString) |> ignore
        diff.Should().BeApproximately(0.0, errInvTolerance, nullString) |> ignore


    [<Fact>]
    member _.odePack_ShouldRunNonLinear () : unit =
        let r = odePackRun EeInfModelData.defaultNonlinearValue (fun _ -> None) defaultNonlinearOdeParams

        let v = r.getData r.result.x
        let inv_tEnd = r.modelData.invariant v
        let diff = (inv_tEnd - r.invStart) / r.invStart

        use _ = new AssertionScope()
        r.result.Should().NotBeNull(nullString) |> ignore
        diff.Should().BeApproximately(0.0, errInvTolerance, nullString) |> ignore


    [<Fact>]
    member _.odePack_ShouldCallBack () : unit =
        let r = odePackRun EeInfModelData.defaultValue (fun _ -> None) defaultOdeParams

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

        let md = EeInfModelData.defaultValue
        let model = EeInfModel.create md.modelParams

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
