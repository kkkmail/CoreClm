namespace OdeSolver

#nowarn "9"

open System
open ClmSys
open ClmSys.ContGenPrimitives
open GenericOdeSolver.Primitives
open Microsoft.FSharp.NativeInterop
open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives
open Primitives.SolverRunnerErrors
open Softellect.OdePackInterop
open Microsoft.FSharp.Core
open Clm.ChartData
open ClmSys.GeneralPrimitives
open ClmSys.GeneralData
open ClmSys.SolverRunnerPrimitives
open ClmSys.ClmErrors
open ClmSys.SolverData
open Clm.CalculationData
open Primitives.GeneralData
open GenericOdeSolver.Solver

module Solver =

    let makeNonNegativeByRef (neq : int) (x : nativeptr<double>) : double[] = [| for i in 0..(neq - 1) -> max 0.0 (NativePtr.get x i) |]

    let makeNonNegativeByRefInPlace (neq : int) (x : nativeptr<double>) =
        for i in 0 .. (neq - 1) do NativePtr.set x i (max 0.0 (NativePtr.get x i))


    let private fUseNonNegative (
                                callBack: double -> double[] -> unit,
                                derivativeCalculator : DerivativeCalculator,
                                neq : byref<int>,
                                t : byref<double>,
                                x : nativeptr<double>,
                                dx : nativeptr<double>) : unit =

        let x1 = makeNonNegativeByRef neq x
        callBack t x1
        let dxValue = derivativeCalculator.calculate t x1
        for i in 0 .. (neq - 1) do NativePtr.set dx i (dxValue[i])


    let private fDoNotCorrect (
                                needsCallBack: double -> CancellationType option * bool,
                                callBack: CancellationType option -> double -> double[] -> unit,
                                derivativeCalculator : DerivativeCalculator,
                                neq : byref<int>,
                                t : byref<double>,
                                x : nativeptr<double>,
                                dx : nativeptr<double>) : unit =

        match needsCallBack t with
        | Some c, _ -> callBack (Some c) t (makeNonNegativeByRef neq x)
        | None, true -> callBack None t (makeNonNegativeByRef neq x)
        | None, false -> ()

        // for i in 0 .. (neq - 1) do
        //     NativePtr.set dx i (calculateByRefDerivativeValue x indices.[i])
        failwith "fDoNotCorrect is not implemented yet."


    let createUseNonNegativeInterop (callaBack: double -> double[] -> unit, derivativeCalculator : DerivativeCalculator) =
        Interop.F(fun n t y dy -> fUseNonNegative(callaBack, derivativeCalculator, &n, &t, y, dy))


    let createDoNotCorrectInterop (
                                    needsCallBack: double -> CancellationType option * bool,
                                    callaBack: CancellationType option -> double -> double[] -> unit,
                                    derivativeCalculator : DerivativeCalculator) =
        Interop.F(fun n t y dy -> fDoNotCorrect(needsCallBack, callaBack, derivativeCalculator, &n, &t, y, dy))


    //let private printDebug s = printfn $"{s}"
    let private printDebug s = ()


    type OdeParams =
        {
            startTime : double
            endTime : double
            stepSize : double
            absoluteTolerance : AbsoluteTolerance
            noOfOutputPoints : int
            noOfProgressPoints : int
            noOfChartDetailedPoints : int option
        }


    type OdeResult =
        {
            startTime : double
            endTime : double
            xEnd : double[]
            progressData : ClmProgressData
        }


    type AlgLibMethod =
        | CashCarp


    type OdePackMethod =
        | Adams
        | Bdf

        member t.value =
            match t with
            | Adams -> 1
            | Bdf -> 2


    type CorrectorIteratorType =
        | Functional
        | ChordWithDiagonalJacobian

        member t.value =
            match t with
            | Functional -> 0
            | ChordWithDiagonalJacobian -> 3


    type NegativeValuesCorrectorType =
        | DoNotCorrect
        | UseNonNegative

        member nc.value =
            match nc with
            | DoNotCorrect -> 0
            | UseNonNegative -> 1


    type SolverType =
        | AlgLib of AlgLibMethod
        | OdePack of OdePackMethod * CorrectorIteratorType * NegativeValuesCorrectorType


    type NSolveParam =
        {
            odeParams : OdeParams
            solverType : SolverType
            modelDataId : Guid
            runQueueId : RunQueueId
            calculationData : ModelCalculationData
            initialValues : double[]
            progressCallBack : RunQueueStatus option -> ClmProgressData -> unit
            chartCallBack : ChartSliceData -> unit
            getChartSliceData : double -> double[] -> EeData -> ChartSliceData
            checkCancellation : RunQueueId -> CancellationType option
            checkFreq : TimeSpan
        }


    type StatUpdateData =
        {
            nSolveParam : NSolveParam
            t : double
            x : double[]
        }

        static member create n =
            {
                nSolveParam = n
                t = 0.0
                x = n.initialValues
            }


    let calculateProgress n t =
        (t - n.odeParams.startTime) / (n.odeParams.endTime - n.odeParams.startTime)
        |> decimal


    let estCompl n t s =
        match estimateEndTime (calculateProgress n t) s with
        | Some e -> " est. compl.: " + e.ToShortDateString() + ", " + e.ToShortTimeString() + ","
        | None -> EmptyString


    let mutable private lastNotifiedT = 0.0
    let mutable private progress = 0.0m
    let mutable private nextProgress = 0.0m
    let mutable private nextChartProgress = 0.0m
    let mutable private callCount = 0L
    let mutable private lastCheck = DateTime.Now
    let mutable private firstChartSliceData = ChartSliceData.defaultValue
    let mutable private lastChartSliceData = ChartSliceData.defaultValue
    let mutable private lastEeData = EeData.defaultValue
    let mutable private tPrev = 0.0
    let mutable private tDtSum = 0.0
    let mutable private dtEeSum = [| 0.0 |]
    let mutable private tDtEeSum = [| 0.0 |]
    let mutable private eeCount = 0
    let mutable calculated = false

    let printProgressInfo t =
        printfn $"printProgressInfo: t = {t}, progress = {progress}, eeData = %0A{lastEeData}."


    let shouldNotifyByCallCount() =
        let r =
            [
                callCount <= 10L
                callCount > 10L && callCount <= 100L && callCount % 5L = 0L
                callCount > 100L && callCount <= 1_000L && callCount % 50L = 0L
                callCount > 1_000L && callCount <= 10_000L && callCount % 500L = 0L
                callCount > 10_000L && callCount <= 100_000L && callCount % 5_000L = 0L
                callCount > 100_000L && callCount <= 1_000_000L && callCount % 50_000L = 0L
                callCount > 1_000_000L && callCount <= 10_000_000L && callCount % 500_000L = 0L
                callCount > 10_000_000L && callCount <= 100_000_000L && callCount % 5_000_000L = 0L
                callCount > 100_000_000L && callCount % 50_000_000L = 0L
            ]
            |> List.tryFind id
            |> Option.defaultValue false

        printDebug $"shouldNotifyByCallCount: callCount = {callCount}, r = {r}."
        r


    let shouldNotifyByNextProgress (n : NSolveParam) t =
        let p = calculateProgress n t
        let r = p >= nextProgress
        printDebug $"shouldNotifyByNextProgress: p = {p}, nextProgress = {nextProgress}, r = {r}."
        r


    let shouldNotifyByNextChartProgress (n : NSolveParam) t =
        let p = calculateProgress n t
        let r = p >= nextChartProgress
        printDebug $"shouldNotifyByNextChart: p = {p}, nextChartProgress = {nextChartProgress}, r = {r}."
        r


    let calculateNextProgress n t =
        let r =
            match n.odeParams.noOfProgressPoints with
            | np when np <= 0 -> 1.0m
            | np -> min 1.0m ((((calculateProgress n t) * (decimal np) |> floor) + 1.0m) / (decimal np))
        printDebug $"calculateNextProgress: r = {r}."
        r

    let calculateNextChartProgress n t =
        let r =
            match n.odeParams.noOfOutputPoints with
            | np when np <= 0 -> 1.0m
            | np -> min 1.0m ((((calculateProgress n t) * (decimal np) |> floor) + 1.0m) / (decimal np))
        printDebug $"calculateNextChartProgress: r = {r}."
        r

    let shouldNotifyProgress n t = shouldNotifyByCallCount() || shouldNotifyByNextProgress n t
    let shouldNotifyChart n t = shouldNotifyByCallCount() || shouldNotifyByNextChartProgress n t


    /// Don't notify twice for the same value of t. This could happen during diagonal Jacobian evaluation.
    let shouldNotify n t = (shouldNotifyProgress n t || shouldNotifyChart n t) && (lastNotifiedT <> t)


    let calculateChartSliceData (d : StatUpdateData) =
        let n = d.nSolveParam
        printDebug $"calculateChartSliceData: Called with t = {d.t}."

        if calculated
        then lastChartSliceData
        else
            let dt = d.t - tPrev
            let tDt = d.t * dt
            let csd = n.getChartSliceData d.t d.x EeData.defaultValue

            let tDtSumNew = tDtSum + tDt
            let dtEeSumNew = (csd.enantiomericExcess, dtEeSum) ||> Array.map2 (fun e s -> dt * e + s)
            let tDtEeSumNew = (csd.enantiomericExcess, tDtEeSum) ||> Array.map2 (fun e s -> tDt * e + s)

            let dtEeAbsMax =
                dtEeSumNew
                |> Array.map abs
                |> Array.max

            let tDtEeAbsMax =
                tDtEeSumNew
                |> Array.map abs
                |> Array.max

            let eeData =
                {
                    maxEe = max lastEeData.maxEe csd.maxEe
                    maxAverageEe = if d.t > 0.0 then dtEeAbsMax / d.t else 0.0
                    maxWeightedAverageAbsEe = if tDtSumNew > 0.0 then tDtEeAbsMax / tDtSumNew else 0.0
                    maxLastEe = csd.maxEe
                }

            tPrev <- d.t
            tDtSum <- tDtSumNew
            dtEeSum <- dtEeSumNew
            tDtEeSum <- tDtEeSumNew
            eeCount <- eeCount + 1
            lastEeData <- eeData
            lastChartSliceData <- { csd with eeData = eeData }
            progress <- calculateProgress d.nSolveParam d.t
            calculated <- true
            csd


    let notifyChart (d : StatUpdateData) =
//        Thread.Sleep(30_000)
        printDebug $"notifyChart: Calling chartCallBack with t = {d.t}."
        calculateChartSliceData d |> d.nSolveParam.chartCallBack


    let calculateProgressData (d : StatUpdateData) =
        printDebug $"calculateProgressData: Called with t = {d.t}."
        let csd = calculateChartSliceData d

        {
            progress = calculateProgress d.nSolveParam d.t
            callCount = callCount
            errorMessageOpt = None
            tx = None
            data =
                {
                    eeData = lastEeData
                    yRelative = csd.totalSubst.totalData / firstChartSliceData.totalSubst.totalData
                }
        }


    let calculateProgressDataWithErr (d : StatUpdateData) v : ClmProgressData =
        printDebug $"calculateProgressDataWithErr: Called with t = {d.t}, v = {v}."
        let p = calculateProgressData d

        let withMessage s m =
            match s with
            | Some v -> { p with errorMessageOpt = m + $" Message: {v}" |> ErrorMessage |> Some }
            | None ->   { p with errorMessageOpt = m |> ErrorMessage |> Some }

        match v with
        | AbortCalculation s -> $"The run queue was aborted at: %.2f{p.progress * 100.0m}%% progress." |> withMessage s
        | CancelWithResults s ->
            $"The run queue was cancelled at: %.2f{p.progress * 100.0m}%% progress. Absolute tolerance: {d.nSolveParam.odeParams.absoluteTolerance}."
            |> withMessage s


    let mapResults n (solverResult : SolverResult) (elapsed : TimeSpan) =
        {
            startTime = solverResult.StartTime
            endTime = solverResult.EndTime
            xEnd = solverResult.X
            progressData = (calculateProgressData { nSolveParam = n; t = solverResult.EndTime; x = solverResult.X })
        }


    let notifyProgress n s p =
        printDebug $"notifyProgress: Called with p = {p}."
//        Thread.Sleep(30_000)
        n.progressCallBack s p


    let checkCancellation n =
        let fromLastCheck = DateTime.Now - lastCheck
        printDebug $"checkCancellation: runQueueId = %A{n.runQueueId}, time interval from last check = %A{fromLastCheck}."

        if fromLastCheck > n.checkFreq
        then
            lastCheck <- DateTime.Now
            let cancel = n.checkCancellation n.runQueueId
            cancel
        else None


    let callBack (d : StatUpdateData) : unit =
        let n = d.nSolveParam
        printDebug $"callBack: Called with t = {d.t}."
        calculated <- false

        let g() =
            progress <- calculateProgress d.nSolveParam d.t
            lastNotifiedT <- d.t
            printProgressInfo d.t

        match shouldNotifyProgress n d.t, shouldNotifyChart n d.t with
        | true, true ->
            calculateProgressData d |> notifyProgress n None
            notifyChart d
            nextProgress <- calculateNextProgress n d.t
            nextChartProgress <- calculateNextChartProgress n d.t
            g()
        | true, false ->
            calculateProgressData d |> notifyProgress n None
            nextProgress <- calculateNextProgress n d.t
            g()
        | false, true ->
            notifyChart d
            nextChartProgress <- calculateNextChartProgress n d.t
            g()
        | false, false -> ()


    let needsCallBack n t =
        let r =
            callCount <- callCount + 1L
            checkCancellation n, shouldNotify n t

        printDebug $"needsCallBack: t = {t}, r = {r}."
        r


    let callBackUseNonNegative (d : StatUpdateData) =
        printDebug $"callBackUseNonNegative: Called with t = {d.t}."
        callCount <- callCount + 1L

        match checkCancellation d.nSolveParam with
        | Some v ->
            let p = calculateProgressDataWithErr d v
            raise(ComputationAbortedException<ClmProgressAdditionalData> (p, v))
        | None -> ()

        if shouldNotify d.nSolveParam d.t then callBack d


    let callBackDoNotCorrect c (d : StatUpdateData) =
        printDebug $"callBackDoNotCorrect: Called with t = {d.t}."

        match c with
        | Some v ->
            let p = calculateProgressDataWithErr d v
            raise(ComputationAbortedException<ClmProgressAdditionalData> (p, v))
        | None -> ()

        if shouldNotify d.nSolveParam d.t then callBack d


    /// F# wrapper around various ODE solvers.
    let nSolve (n : NSolveParam) : OdeResult =
        printfn "nSolve::Starting."
        let p = n.odeParams

        let callBackUseNonNegative t x = callBackUseNonNegative { nSolveParam = n; t = t; x = x }

        let d = StatUpdateData.create n
        let csd = n.getChartSliceData d.t d.x EeData.defaultValue
        lastChartSliceData <- csd
        dtEeSum <- csd.enantiomericExcess |> Array.map (fun _ -> 0.0)
        tDtEeSum <- csd.enantiomericExcess |> Array.map (fun _ -> 0.0)
        firstChartSliceData <- calculateChartSliceData d
        calculateProgressData d |> notifyProgress n (Some InProgressRunQueue)

        match n.solverType with
        | AlgLib CashCarp ->
            printfn "nSolve: Using Cash - Carp Alglib solver."
            let nt = 2

            let cashCarpDerivative (x : double[]) (t : double) : double[] =
                callBackUseNonNegative t x
                n.calculationData.getDerivative x

            let x : array<double> = [| for i in 0..nt -> p.startTime + (p.endTime - p.startTime) * (double i) / (double nt) |]
            let d = alglib.ndimensional_ode_rp (fun x t y _ -> cashCarpDerivative x t |> Array.mapi(fun i e -> y.[i] <- e) |> ignore)
            let mutable s = alglib.odesolverrkck(n.initialValues, x, p.absoluteTolerance.value, p.stepSize)
            do alglib.odesolversolve(s, d, null)
            let mutable (m, xTbl, yTbl, rep) = alglib.odesolverresults(s)
            let xEnd = yTbl.[nt - 1, *]
            let pd = calculateProgressData { nSolveParam = n; t = p.endTime; x = xEnd }

            {
                startTime = p.startTime
                endTime = p.endTime
                xEnd = xEnd
                progressData = pd
            }
        | OdePack (m, i, nc) ->
            printfn $"nSolve: Using {m} / {i} DLSODE solver."
            let calculateDerivative t x = n.calculationData.getDerivative x
            let dc : DerivativeCalculator = calculateDerivative |> FullArray

            match nc with
            | UseNonNegative ->
                let interop() = createUseNonNegativeInterop(callBackUseNonNegative, dc)

                OdeSolver.RunFSharp(
                        (fun() -> interop()),
                        m.value,
                        i.value,
                        p.startTime,
                        p.endTime,
                        n.initialValues,
                        (fun r e -> mapResults n r e),
                        p.absoluteTolerance.value)

            | DoNotCorrect ->
                let needsCallBack t = needsCallBack n t
                let callBack c t x = callBackDoNotCorrect c { nSolveParam = n; t = t; x = x }
                let interop() = createDoNotCorrectInterop(needsCallBack, callBack, dc)

                OdeSolver.RunFSharp(
                        (fun() -> interop()),
                        m.value,
                        i.value,
                        p.startTime,
                        p.endTime,
                        n.initialValues,
                        (fun r e -> mapResults n r e),
                        p.absoluteTolerance.value)
