namespace OdeSolver

open System
open ClmSys.ContGenPrimitives
open Softellect.OdePackInterop
open Microsoft.FSharp.Core
open Clm.ChartData
open ClmSys.GeneralPrimitives
open ClmSys.GeneralData
open ClmSys.SolverRunnerPrimitives
open ClmSys.ClmErrors
open ClmSys.SolverData
open Clm.CalculationData

module Solver =

//    let private printDebug s = printfn $"{s}"
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
            progressData : ProgressData
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
            progressCallBack : RunQueueStatus option -> ProgressData -> unit
            chartCallBack : ChartSliceData -> unit
            getChartSliceData : double -> double[] -> ChartSliceData
            checkCancellation : RunQueueId -> CancellationType option
            checkFreq : TimeSpan
        }

//        member p.next tEndNew initValNew = { p with tStart = p.tEnd; tEnd = tEndNew; initialValues = initValNew }


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
    let mutable private lastTimeCheck = lastCheck
    let mutable private firstChartSliceData = ChartSliceData.defaultValue
    let mutable private lastChartSliceData = ChartSliceData.defaultValue
    let mutable private lastEeData = EeData.defaultValue
    let mutable private tSum = 0.0
    let mutable private eeCount = 0
    let mutable calculated = false

    let printProgressInfo t =
        printfn $"t = {t}, progress = {progress}, eeData = %0A{lastEeData}."


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


    let calculateChartSliceData n t x =
        printDebug $"calculateChartSliceData: Called with t = {t}."
        if calculated
        then lastChartSliceData
        else
            let csd = n.getChartSliceData t x

            // TODO kk:20210317 - This is not completely correct - figure out what's wrong and fix.
            let eeData =
                {
                    maxEe = max lastEeData.maxEe csd.maxEe
                    maxAverageEe = (lastEeData.maxAverageEe * (double eeCount) + csd.maxEe) / ((double eeCount) + 1.0)
                    maxWeightedAverageAbsEe = if t > n.odeParams.startTime then (lastEeData.maxWeightedAverageAbsEe * tSum + csd.maxEe * t) / (tSum + t) else 0.0
                    maxLastEe = csd.maxEe
                }

            tSum <- tSum + t
            eeCount <- eeCount + 1
            lastEeData <- eeData
            lastChartSliceData <- csd
            csd


    let notifyChart n t x =
//        Thread.Sleep(30_000)
        printDebug $"notifyChart: Calling chartCallBack with t = {t}."
        calculateChartSliceData n t x |> n.chartCallBack


    let calculateProgressData n t x =
        printDebug $"calculateProgressData: Called with t = {t}."
        let csd = calculateChartSliceData n t x

        {
            progress = calculateProgress n t
            callCount = callCount
            eeData = lastEeData
            yRelative = csd.totalSubst.totalData / firstChartSliceData.totalSubst.totalData
            errorMessageOpt = None
        }


    let calculateProgressDataWithErr n t x v =
        printDebug $"calculateProgressDataWithErr: Called with t = {t}, v = {v}."
        let p = calculateProgressData n t x

        let withMessage s m =
            match s with
            | Some v -> { p with errorMessageOpt = m + $" Message: {v}" |> ErrorMessage |> Some }
            | None ->   { p with errorMessageOpt = m |> ErrorMessage |> Some }

        match v with
        | AbortCalculation s -> $"The run queue was aborted at: %.2f{p.progress * 100.0m}%% progress." |> withMessage s
        | CancelWithResults s ->
            $"The run queue was cancelled at: %.2f{p.progress * 100.0m}%% progress. Absolute tolerance: {n.odeParams.absoluteTolerance}."
            |> withMessage s


    let mapResults n (solverResult : SolverResult) (elapsed : TimeSpan) =
        {
            startTime = solverResult.StartTime
            endTime = solverResult.EndTime
            xEnd = solverResult.X
            progressData = calculateProgressData n solverResult.EndTime solverResult.X
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


    let callBack n (t : double) (x : double[]) : unit =
        printDebug $"callBack: Called with t = {t}."
        calculated <- false

        let g() =
            lastNotifiedT <- t
            printProgressInfo t

        match shouldNotifyProgress n t, shouldNotifyChart n t with
        | true, true ->
            calculateProgressData n t x |> notifyProgress n None
            notifyChart n t x
            nextProgress <- calculateNextProgress n t
            nextChartProgress <- calculateNextChartProgress n t
            g()
        | true, false ->
            calculateProgressData n t x |> notifyProgress n None
            nextProgress <- calculateNextProgress n t
            g()
        | false, true ->
            notifyChart n t x
            nextChartProgress <- calculateNextChartProgress n t
            g()
        | false, false -> ()


    let needsCallBack n t =
        let r =
            callCount <- callCount + 1L
            checkCancellation n, shouldNotify n t

        printDebug $"needsCallBack: t = {t}, r = {r}."
        r


    let callBackUseNonNegative n t x =
        printDebug $"callBackUseNonNegative: Called with t = {t}."
        callCount <- callCount + 1L

        match checkCancellation n with
        | Some v -> raise(ComputationAbortedException (calculateProgressDataWithErr n t x v, v))
        | None -> ()

        if shouldNotify n t then callBack n t x


    let callBackDoNotCorrect n c t x =
        printDebug $"callBackDoNotCorrect: Called with t = {t}."

        match c with
        | Some v -> raise(ComputationAbortedException (calculateProgressDataWithErr n t x v, v))
        | None -> ()

        if shouldNotify n t then callBack n t x


    /// F# wrapper around various ODE solvers.
    let nSolve (n : NSolveParam) : OdeResult =
        printfn "nSolve::Starting."
        let p = n.odeParams
        let callBackUseNonNegative t x = callBackUseNonNegative n t x
        firstChartSliceData <- calculateChartSliceData n 0.0 n.initialValues

        calculateProgressData n n.odeParams.startTime n.initialValues |> notifyProgress n (Some InProgressRunQueue)

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

            {
                startTime = p.startTime
                endTime = p.endTime
                xEnd = xEnd
                progressData = calculateProgressData n p.endTime xEnd
            }
        | OdePack (m, i, nc) ->
            printfn $"nSolve: Using {m} / {i} DLSODE solver."

            match nc with
            | UseNonNegative ->
                let interop() = createUseNonNegativeInterop(callBackUseNonNegative, n.calculationData.modelIndices)

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
                let callBack c t x = callBackDoNotCorrect n c t x
                let interop() = createDoNotCorrectInterop(needsCallBack, callBack, n.calculationData.modelIndices)

                OdeSolver.RunFSharp(
                        (fun() -> interop()),
                        m.value,
                        i.value,
                        p.startTime,
                        p.endTime,
                        n.initialValues,
                        (fun r e -> mapResults n r e),
                        p.absoluteTolerance.value)
