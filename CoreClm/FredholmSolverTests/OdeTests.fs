namespace FredholmSolverTests

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
open Xunit
open Xunit.Abstractions

type private CallBackResults =
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


/// TODO kk:20230413 - Consolidate data creation into some common place.
type OdeTests (output : ITestOutputHelper) =
    let writeLine s = output.WriteLine s
    let nullString : string = null
    let errTolerance = 1.0e-10
    let errInvTolerance = 1.0e-3

    let defaultKernelData =
        {
            noOfIntervals = 101
            l2 = 25
            zeroThreshold = MutationProbabilityData.defaultZeroThreshold
            epsEeFunc = (fun _ -> 0.02) |> EpsFunc
            epsInfFunc = (fun _ -> 0.02) |> EpsFunc
            kaFunc = (fun _ _ _ -> 1.0) |> KaFunc
        }

    let domain2D data = Domain2D.create data.noOfIntervals data.l2

    /// Creates a "delta" function centered near (0, 0) in the domain,
    /// which is a middle point in ee domain and 0-th point in inf domain.
    let getDeltaU data =
        let domain =
            if data.noOfIntervals % 2 = 1
            then domain2D data
            else failwith "data.noOfIntervals must be odd for this method to work."

        let g i j = if (i * 2 + 1 = data.noOfIntervals) && (j = 0) then 1.0 else 0.0
        let v = domain.eeDomain.midPoints.value |> Array.mapi (fun i _ -> domain.infDomain.midPoints.value |> Array.mapi (fun j _ -> g i j)) |> Matrix
        let norm = domain.integrateValues v
        (1.0 / norm) * v

    let initialValues data =
        let f = FoodData 10.0
        let w = WasteData 0.0
        let u = getDeltaU data |> ProtocellData
        let sd = SubstanceData.create f w u
        sd


    let modelData data : ModelData =
        let kernel = Kernel.create data

        let gamma =
            kernel.domainData.eeDomain.midPoints.value
            |> Array.map (fun a -> kernel.domainData.infDomain.midPoints.value |> Array.map (fun b -> 0.01))
            |> Matrix
            |> Gamma

        {
            kernel = kernel
            gamma = gamma
            n = NumberOfMolecules 100
            s = RecyclingRate 0.1
        }


    let odeParams =
        {
            startTime = 0.0
            endTime = 10.0
            stepSize = 1.0e-3
            absoluteTolerance = AbsoluteTolerance.defaultValue
            solverType = OdePack (Bdf, ChordWithDiagonalJacobian, UseNonNegative)
            outputParams =
                {
                    noOfOutputPoints = 100
                    noOfProgressPoints = 2
                    noOfChartDetailedPoints = None
                }
        }

    let outputResult md t (v : SubstanceData) =
        let u = v.protocell
        let total = md.kernel.domainData.integrateValues u
        let inv = md.invariant v
        let m = md.kernel.domainData.mean u
        let s = md.kernel.domainData.stdDev u
        writeLine $"t: {t}, inv: {inv}, total: {total}, mean: {m}, stdDev: {s}."

    let nSolveParam data =
        let i = initialValues data
        let md = modelData data
        let f t v = outputResult md t v
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
                        needsCallBack = NeedsCallBack (fun c _ -> c, None)
                        progressCallBack = ProgressCallBack (fun _ _ _ -> ())
                        chartCallBack = ChartCallBack (fun _ _ _ -> ())
                        checkCancellation = CheckCancellation (fun _ -> None)
                    }

                started = DateTime.Now
            }

        n, v


    let callBack cr c _ _ =
        match c with
        | RegularCallBack -> { cr with progressCallBackCount = cr.progressCallBackCount + 1 }
        | FinalCallBack ct ->
            match ct with
            | CompletedCalculation -> { cr with completedCallBackCount = cr.completedCallBackCount + 1 }
            | CancelledCalculation c ->
                match c with
                | CancelWithResults _  -> { cr with cancelledCallBackCount = cr.cancelledCallBackCount + 1 }
                | AbortCalculation _ -> { cr with abortedCallBackCount = cr.abortedCallBackCount + 1 }


    let charCallBack cr _ _ _ = { cr with chartCallBackCount = cr.chartCallBackCount + 1 }


    let calLBackInfo n callBack charCallBack checkCancellation =
        {
            checkFreq = TimeSpan.FromMilliseconds(10.0)
            needsCallBack = n.odeParams.outputParams.needsCallBack n
            progressCallBack = ProgressCallBack callBack
            chartCallBack = ChartCallBack charCallBack
            checkCancellation = CheckCancellation checkCancellation
        }


    [<Fact>]
    member _.odePack_ShouldRun () : unit =
        let data = defaultKernelData
        let md = modelData data
        let nSolveParam, getData = nSolveParam data
        let inv_tStart = getData nSolveParam.initialValues |> md.invariant
        let result = nSolve nSolveParam
        let v = getData result.xEnd
        let inv_tEnd = md.invariant v
        let diff = (inv_tEnd - inv_tStart) / inv_tStart
        outputResult md result.endTime v
        writeLine $"result: {result}."
        result.Should().NotBeNull(nullString) |> ignore
        diff.Should().BeApproximately(0.0, errInvTolerance, nullString) |> ignore


    [<Fact>]
    member _.odePack_ShouldCallBack () : unit =
        let mutable cr = CallBackResults.defaultValue

        let data = defaultKernelData
        let n, _ = nSolveParam data

        let callBack c t x = cr <- callBack cr c t x
        let charCallBack c t x = cr <- charCallBack cr c t x
        let c = calLBackInfo n callBack charCallBack (fun _ -> None)

        let nSolveParam = { n with callBackInfo = c }
        let result = nSolve nSolveParam

        use _ = new AssertionScope()
        result.Should().NotBeNull(nullString) |> ignore
        cr.progressCallBackCount.Should().BeGreaterThan(0, nullString) |> ignore
        cr.completedCallBackCount.Should().BeGreaterThan(0, nullString) |> ignore
        cr.cancelledCallBackCount.Should().Be(0, nullString) |> ignore
        cr.chartCallBackCount.Should().BeGreaterThan(0, nullString) |> ignore


    [<Fact>]
    member _.odePack_ShouldCancel () : unit =
        let mutable cr = CallBackResults.defaultValue

        let data = defaultKernelData
        let n, _ = nSolveParam data

        let callBack c t x = cr <- callBack cr c t x
        let charCallBack c t x = cr <- charCallBack cr c t x
        let checkCancellation _ = CancelWithResults None |> Some
        let c = calLBackInfo n callBack charCallBack checkCancellation

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

        let data = defaultKernelData
        let n, _ = nSolveParam data

        let callBack c t x = cr <- callBack cr c t x
        let charCallBack c t x = cr <- charCallBack cr c t x
        let checkCancellation _ = AbortCalculation None |> Some
        let c = calLBackInfo n callBack charCallBack checkCancellation

        let nSolveParam = { n with callBackInfo = c }

        use _ = new AssertionScope()
        FluentActions.Invoking(fun () -> nSolve nSolveParam |> ignore).Should().Throw<ComputationAbortedException>(nullString) |> ignore
        cr.progressCallBackCount.Should().BeGreaterThan(0, nullString) |> ignore
        cr.completedCallBackCount.Should().Be(0, nullString) |> ignore
        cr.cancelledCallBackCount.Should().Be(0, nullString) |> ignore
        cr.abortedCallBackCount.Should().Be(1, nullString) |> ignore
