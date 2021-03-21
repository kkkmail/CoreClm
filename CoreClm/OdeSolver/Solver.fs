namespace OdeSolver

open System
open System.Threading
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

    type OdeParams =
        {
            startTime : double
            endTime : double
            stepSize : double
            eps : double
            noOfOutputPoints : int
            noOfProgressPoints : int
        }

        static member defaultValue startTime endTime op pp =
            {
                startTime = startTime
                endTime = endTime
                stepSize = 0.1
                eps = 0.000_01
                noOfOutputPoints = op
                noOfProgressPoints = pp
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
            solverType : SolverType
            modelDataId : Guid
            runQueueId : RunQueueId
            tStart : double
            tEnd : double
            calculationData : ModelCalculationData
            initialValues : double[]
            progressCallBack : RunQueueStatus option -> ProgressData -> unit
            chartCallBack : ChartSliceData -> unit
            getChartSliceData : double -> double[] -> ChartSliceData
            noOfOutputPoints : int
            noOfProgressPoints : int
            noOfChartDetailedPoints : int option
            checkCancellation : RunQueueId -> CancellationType option
            checkFreq : TimeSpan
        }

        member p.next tEndNew initValNew = { p with tStart = p.tEnd; tEnd = tEndNew; initialValues = initValNew }


    let calculateProgress n t = (t - n.tStart) / (n.tEnd - n.tStart)


    let estCompl n t s =
        match estimateEndTime (calculateProgress n t) s with
        | Some e -> " est. compl.: " + e.ToShortDateString() + ", " + e.ToShortTimeString() + ","
        | None -> EmptyString


    let mutable private progress = 0.0
    let mutable private nextProgress = 0.0
    let mutable private outputCount = 0
    let mutable private callCount = 0L
    let mutable private lastCheck = DateTime.Now
    let mutable private lastTimeCheck = lastCheck
    let mutable private firstChartSliceData = ChartSliceData.defaultValue
    let mutable private lastChartSliceData = ChartSliceData.defaultValue
    let mutable private lastEeData = EeData.defaultValue
    let mutable private tSum = 0.0
    let mutable private eeCount = 0
    let mutable calculated = false


    let shouldNotifyByCallCount() =
        let r =
            [
                callCount <= 100L && callCount % 5L = 0L
                callCount > 100L && callCount <= 1_000L && callCount % 50L = 0L
                callCount > 1_000L && callCount <= 10_000L && callCount % 500L = 0L
                callCount > 10_000L && callCount <= 100_000L && callCount % 5_000L = 0L
                callCount > 100_000L && callCount <= 1_000_000L && callCount % 50_000L = 0L
                callCount > 1_000_000L && callCount <= 10_000_000L && callCount % 500_000L = 0L
                callCount > 10_000_000L && callCount % 5_000_000L = 0L
            ]
            |> List.tryFind id
            |> Option.defaultValue false

//        printfn $"shouldNotifyByCallCount: callCount = {callCount}, r = {r}."
        r


    let shouldNotifyByNextProgress (n : NSolveParam) t =
        let p = calculateProgress n t
        let r = p >= nextProgress
//        printfn $"shouldNotifyByNextProgress: p = {p}, nextProgress = {nextProgress}, r = {r}."
        r


    let calculateNextProgress n t =
        let r =
            match n.noOfProgressPoints with
            | np when np <= 0 -> 1.0
            | np -> min 1.0 ((((calculateProgress n t) * (double np) |> floor) + 1.0) / (double np))
//        printfn $"calculateNextProgress: r = {r}."
        r

    let shouldNotifyChart n t =
        let r =
            match n.noOfOutputPoints with
            | np when np <= 0 -> false
            | np ->
                match n.noOfChartDetailedPoints with
                | Some cp -> ((double np) * nextProgress <= double cp && shouldNotifyByCallCount()) || shouldNotifyByNextProgress n t
                | None -> shouldNotifyByNextProgress n t
//        printfn $"shouldNotifyChart: n.noOfOutputPoints = {n.noOfOutputPoints}, n.noOfChartDetailedPoints = {n.noOfChartDetailedPoints}, r = {r}."
        r


    let shouldNotifyProgress n t = shouldNotifyByCallCount() || shouldNotifyByNextProgress n t
    let shouldNotify n t = shouldNotifyProgress n t || shouldNotifyChart n t


    let calculateChartSliceData n t x =
//        printfn $"calculateChartSliceData: Called with t = {t}."
        if calculated
        then lastChartSliceData
        else
            let csd = n.getChartSliceData t x

            // TODO kk:20210317 - This is not completely correct - figure out what's wrong and fix.
            let eeData =
                {
                    maxEe = max lastEeData.maxEe csd.maxEe
                    maxAverageEe = (lastEeData.maxAverageEe * (double eeCount) + csd.maxEe) / ((double eeCount) + 1.0)
                    maxWeightedAverageAbsEe = if t > n.tStart then (lastEeData.maxWeightedAverageAbsEe * tSum + csd.maxEe * t) / (tSum + t) else 0.0
                    maxLastEe = csd.maxEe
                }

            tSum <- tSum + t
            eeCount <- eeCount + 1
            lastEeData <- eeData
            lastChartSliceData <- csd
            csd


    let notifyChart n t x =
//        Thread.Sleep(30_000)
//        printfn $"notifyChart: Calling chartCallBack with t = {t}."
        calculateChartSliceData n t x |> n.chartCallBack


    let calculateProgressData n t x =
//        printfn $"calculateProgressData: Called with t = {t}."
        let csd = calculateChartSliceData n t x

        {
            progress = calculateProgress n t
            callCount = callCount
            eeData = lastEeData
            yRelative = csd.totalSubst.totalData / firstChartSliceData.totalSubst.totalData
            errorMessageOpt = None
        }


    let calculateProgressDataWithErr n t x v =
//        printfn $"calculateProgressDataWithErr: Called with t = {t}, v = {v}."
        let p = calculateProgressData n t x

        match v with
        | CancelWithResults s ->
            let m = $"The run queue was cancelled at: %.2f{p.progress * 100.0}%% progress."

            match s with
            | Some v -> { p with errorMessageOpt = m + $" Message: {v}" |> ErrorMessage |> Some }
            | None -> { p with errorMessageOpt = m |> ErrorMessage |> Some }
        | AbortCalculation s ->
            let m = $"The run queue was aborted at: %.2f{p.progress * 100.0}%% progress."

            match s with
            | Some v -> { p with errorMessageOpt = m + $" Message: {v}" |> ErrorMessage |> Some }
            | None -> { p with errorMessageOpt = m |> ErrorMessage |> Some }


    let mapResults n (solverResult : SolverResult) (elapsed : TimeSpan) =
        {
            startTime = solverResult.StartTime
            endTime = solverResult.EndTime
            xEnd = solverResult.X
            progressData = calculateProgressData n solverResult.EndTime solverResult.X
        }


    let notifyProgress n p =
//        printfn $"notifyProgress: Called with p = {p}."
//        Thread.Sleep(30_000)
        n.progressCallBack None p


    let checkCancellation n =
        let fromLastCheck = DateTime.Now - lastCheck
//        printfn $"checkCancellation: runQueueId = %A{n.runQueueId}, time interval from last check = %A{fromLastCheck}."

        if fromLastCheck > n.checkFreq
        then
            lastCheck <- DateTime.Now
            let cancel = n.checkCancellation n.runQueueId
            cancel
        else None


    let callBack n (t : double) (x : double[]) : unit =
//        printfn $"callBack: Called with t = {t}."
        calculated <- false

        match shouldNotifyProgress n t, shouldNotifyChart n t with
        | true, true ->
            calculateProgressData n t x |> notifyProgress n
            notifyChart n t x
            nextProgress <- calculateNextProgress n t
        | true, false ->
            calculateProgressData n t x |> notifyProgress n
            nextProgress <- calculateNextProgress n t
        | false, true ->
            notifyChart n t x
            nextProgress <- calculateNextProgress n t
        | false, false -> ()


    let needsCallBack n t =
        let r =
            callCount <- callCount + 1L
            checkCancellation n, shouldNotify n t

//        printfn $"needsCallBack: t = {t}, r = {r}."
        r


    let callBackUseNonNegative n t x =
//        printfn $"callBackUseNonNegative: Called with t = {t}."
        callCount <- callCount + 1L

        match checkCancellation n with
        | Some v -> raise(ComputationAbortedException (calculateProgressDataWithErr n t x v, v))
        | None -> ()

        if shouldNotify n t then callBack n t x


    let callBackDoNotCorrect n c t x =
//        printfn $"callBackDoNotCorrect: Called with t = {t}."

        match c with
        | Some v -> raise(ComputationAbortedException (calculateProgressDataWithErr n t x v, v))
        | None -> ()

        if shouldNotify n t then callBack n t x


    /// F# wrapper around various ODE solvers.
    let nSolve (n : NSolveParam) : OdeResult =
        printfn "nSolve::Starting."
        let p = OdeParams.defaultValue n.tStart n.tEnd n.noOfOutputPoints n.noOfProgressPoints
        let callBackUseNonNegative t x = callBackUseNonNegative n t x
        firstChartSliceData <- calculateChartSliceData n 0.0 n.initialValues

        calculateProgressData n n.tStart n.initialValues |> notifyProgress n

        match n.solverType with
        | AlgLib CashCarp ->
            printfn "nSolve: Using Cash - Carp Alglib solver."
            let nt = 2

            let cashCarpDerivative (x : double[]) (t : double) : double[] =
                callBackUseNonNegative t x
                n.calculationData.getDerivative x

            let x : array<double> = [| for i in 0..nt -> p.startTime + (p.endTime - p.startTime) * (double i) / (double nt) |]
            let d = alglib.ndimensional_ode_rp (fun x t y _ -> cashCarpDerivative x t |> Array.mapi(fun i e -> y.[i] <- e) |> ignore)
            let mutable s = alglib.odesolverrkck(n.initialValues, x, p.eps, p.stepSize)
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
                OdeSolver.RunFSharp((fun() -> interop()), m.value, i.value, p.startTime, p.endTime, n.initialValues, (fun r e -> mapResults n r e))
            | DoNotCorrect ->
                let needsCallBack t = needsCallBack n t
                let callBack c t x = callBackDoNotCorrect n c t x
                let interop() = createDoNotCorrectInterop(needsCallBack, callBack, n.calculationData.modelIndices)
                OdeSolver.RunFSharp((fun() -> interop()), m.value, i.value, p.startTime, p.endTime, n.initialValues, (fun r e -> mapResults n r e))
