namespace GenericOdeSolver

open System
// open ClmSys.ContGenPrimitives
open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives
open Softellect.OdePackInterop
open Microsoft.FSharp.Core
open Primitives.GeneralData
// open Clm.ChartData
// open ClmSys.GeneralPrimitives
// open ClmSys.GeneralData
// open ClmSys.SolverRunnerPrimitives
// open ClmSys.ClmErrors
// open ClmSys.SolverData
// open Clm.CalculationData

module Solver =

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


    type OdeResult<'PD> =
        {
            startTime : double
            endTime : double
            xEnd : double[]
            progressData : ProgressData<'PD>
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


    type NSolveParam<'CD, 'PD, 'CSD, 'ED> =
        {
            odeParams : OdeParams
            solverType : SolverType
            modelDataId : Guid
            runQueueId : RunQueueId
            initialValues : double[]

            calculationData : 'CD
            defaultProgressData : 'PD
            defaultChartSliceData : 'CSD
            defaultExtraData : 'ED

            checkFreq : TimeSpan
            progressCallBack : RunQueueStatus option -> ProgressData<'PD> -> unit
            chartCallBack : 'CSD -> unit
            getChartSliceData : double -> double[] -> 'PD -> 'CSD
            checkCancellation : RunQueueId -> CancellationType option
            getExtraData : unit -> 'ED
        }


    // type StatUpdateData<'CD, 'PD, 'CSD, 'ED> =
    //     {
    //         nSolveParam : NSolveParam<'CD, 'PD, 'CSD, 'ED>
    //         t : double
    //         x : double[]
    //     }
    //
    //     static member create n =
    //         {
    //             nSolveParam = n
    //             t = 0.0
    //             x = n.initialValues
    //         }


    type private ProgressStateData<'CD, 'PD, 'CSD, 'ED> =
        {
            nSolveParam : NSolveParam<'CD, 'PD, 'CSD, 'ED>
            t : double
            x : double[]

            lastNotifiedT : double
            progress : decimal
            nextProgress : decimal
            nextChartProgress : decimal
            callCount : int64
            lastCheck  : DateTime
            started : DateTime

            firstChartSliceData : 'CSD
            lastChartSliceData : 'CSD
            lastProgressData : 'PD
            extraData : 'ED

            tPrev : double
            tDtSum : double
            calculated : bool
        }

        static member create n =
            {
                nSolveParam = n
                t = 0.0
                x =  n.initialValues

                lastNotifiedT = 0.0
                progress = 0.0m
                nextProgress = 0.0m
                nextChartProgress = 0.0m
                callCount = 0L
                lastCheck = DateTime.Now
                started = DateTime.Now

                firstChartSliceData = n.defaultChartSliceData
                lastChartSliceData = n.defaultChartSliceData
                lastProgressData = n.defaultProgressData
                extraData = n.defaultExtraData

                tPrev = 0.0
                tDtSum = 0.0
                calculated = false
            }



    let calculateProgress psd =
        (psd.t - psd.nSolveParam.odeParams.startTime) / (psd.nSolveParam.odeParams.endTime - psd.nSolveParam.odeParams.startTime)
        |> decimal


    let estCompl psd =
        match estimateEndTime (calculateProgress psd) psd.started with
        | Some e -> " est. compl.: " + e.ToShortDateString() + ", " + e.ToShortTimeString() + ","
        | None -> EmptyString


    // let mutable private lastNotifiedT = 0.0
    // let mutable private progress = 0.0m
    // let mutable private nextProgress = 0.0m
    // let mutable private nextChartProgress = 0.0m
    // let mutable private callCount = 0L
    // let mutable private lastCheck = DateTime.Now
    // let mutable private tPrev = 0.0
    // let mutable private tDtSum = 0.0
    // let mutable calculated = false

    // 'CSD and 'CSD
    // let mutable private firstChartSliceData = ChartSliceData.defaultValue
    // let mutable private lastChartSliceData = ChartSliceData.defaultValue

    // 'PD
    // let mutable private lastEeData = EeData.defaultValue

    // 'ED
    // let mutable private dtEeSum = [| 0.0 |]
    // let mutable private tDtEeSum = [| 0.0 |]
    // let mutable private eeCount = 0


    let printProgressInfo psd =
        printfn $"printProgressInfo: t = {psd.t}, progress = {psd.progress}, lastProgressData = %0A{psd.lastProgressData}."


    let shouldNotifyByCallCount psd =
        let r =
            [
                psd.callCount <= 10L
                psd.callCount > 10L && psd.callCount <= 100L && psd.callCount % 5L = 0L
                psd.callCount > 100L && psd.callCount <= 1_000L && psd.callCount % 50L = 0L
                psd.callCount > 1_000L && psd.callCount <= 10_000L && psd.callCount % 500L = 0L
                psd.callCount > 10_000L && psd.callCount <= 100_000L && psd.callCount % 5_000L = 0L
                psd.callCount > 100_000L && psd.callCount <= 1_000_000L && psd.callCount % 50_000L = 0L
                psd.callCount > 1_000_000L && psd.callCount <= 10_000_000L && psd.callCount % 500_000L = 0L
                psd.callCount > 10_000_000L && psd.callCount <= 100_000_000L && psd.callCount % 5_000_000L = 0L
                psd.callCount > 100_000_000L && psd.callCount % 50_000_000L = 0L
            ]
            |> List.tryFind id
            |> Option.defaultValue false

        printDebug $"shouldNotifyByCallCount: callCount = {psd.callCount}, r = {r}."
        r


    let shouldNotifyByNextProgress psd =
        let p = calculateProgress psd
        let r = p >= psd.nextProgress
        printDebug $"shouldNotifyByNextProgress: p = {p}, nextProgress = {psd.nextProgress}, r = {r}."
        r


    let shouldNotifyByNextChartProgress psd =
        let p = calculateProgress psd
        let r = p >= psd.nextChartProgress
        printDebug $"shouldNotifyByNextChart: p = {p}, nextChartProgress = {psd.nextChartProgress}, r = {r}."
        r


    let calculateNextProgress psd =
        let r =
            match psd.nSolveParam.odeParams.noOfProgressPoints with
            | np when np <= 0 -> 1.0m
            | np -> min 1.0m ((((calculateProgress psd) * (decimal np) |> floor) + 1.0m) / (decimal np))

        printDebug $"calculateNextProgress: r = {r}."
        r

    let calculateNextChartProgress psd =
        let r =
            match psd.nSolveParam.odeParams.noOfOutputPoints with
            | np when np <= 0 -> 1.0m
            | np -> min 1.0m ((((calculateProgress psd) * (decimal np) |> floor) + 1.0m) / (decimal np))

        printDebug $"calculateNextChartProgress: r = {r}."
        r

    let shouldNotifyProgress psd = shouldNotifyByCallCount psd || shouldNotifyByNextProgress psd
    let shouldNotifyChart psd = shouldNotifyByCallCount psd || shouldNotifyByNextChartProgress psd


    /// Don't notify twice for the same value of t. This could happen during diagonal Jacobian evaluation.
    let shouldNotify psd = (shouldNotifyProgress psd || shouldNotifyChart psd) && (psd.lastNotifiedT <> psd.t)


    let calculateChartSliceData psd =
        let n = psd.nSolveParam
        printDebug $"calculateChartSliceData: Called with t = {psd.t}."

        if psd.calculated
        then psd.lastChartSliceData
        else
            let dt = psd.t - psd.tPrev
            let tDt = psd.t * dt
            let csd = n.getChartSliceData psd.t psd.x n.defaultProgressData

            let tDtSumNew = psd.tDtSum + tDt
            let extraData = n.getExtraData ()
            // let dtEeSumNew = (csd.enantiomericExcess, dtEeSum) ||> Array.map2 (fun e s -> dt * e + s)
            // let tDtEeSumNew = (csd.enantiomericExcess, tDtEeSum) ||> Array.map2 (fun e s -> tDt * e + s)
            //
            // let dtEeAbsMax =
            //     dtEeSumNew
            //     |> Array.map abs
            //     |> Array.max
            //
            // let tDtEeAbsMax =
            //     tDtEeSumNew
            //     |> Array.map abs
            //     |> Array.max
            //
            // let eeData =
            //     {
            //         maxEe = max lastEeData.maxEe csd.maxEe
            //         maxAverageEe = if d.t > 0.0 then dtEeAbsMax / d.t else 0.0
            //         maxWeightedAverageAbsEe = if tDtSumNew > 0.0 then tDtEeAbsMax / tDtSumNew else 0.0
            //         maxLastEe = csd.maxEe
            //     }

            let psdNew =
                {
                    psd
                        with
                        // tPrev = d.t
                        tDtSum = tDtSumNew
                        // dtEeSum <- dtEeSumNew
                        // tDtEeSum = tDtEeSumNew
                        // eeCount <- eeCount + 1
                        // lastEeData <- eeData
                        // lastChartSliceData = { csd with eeData = eeData }
                        progress = calculateProgress psd
                        calculated = true
                }

            // dtEeSum <- dtEeSumNew
            // tDtEeSum <- tDtEeSumNew
            // eeCount <- eeCount + 1
            // lastEeData <- eeData
            // lastChartSliceData <- { csd with eeData = eeData }
            // progress <- calculateProgress d.nSolveParam d.t
            // calculated <- true

            // (csd, psdNew)
            psdNew


    let notifyChart psd =
//        Thread.Sleep(30_000)
        printDebug $"notifyChart: Calling chartCallBack with t = {psd.t}."
        let psdNew = calculateChartSliceData psd
        psd.lastChartSliceData |> psd.nSolveParam.chartCallBack


    let calculateProgressData psd =
        printDebug $"calculateProgressData: Called with t = {psd.t}."
        let psdNew = calculateChartSliceData psd

        {
            progress = calculateProgress psd
            callCount = psd.callCount
            progressData = psd.lastProgressData
            yRelative = csd.totalSubst.totalData / firstChartSliceData.totalSubst.totalData
            errorMessageOpt = None
        }


    let calculateProgressDataWithErr psd v =
        printDebug $"calculateProgressDataWithErr: Called with t = {psd.t}, v = {v}."
        let p = calculateProgressData psd

        let withMessage s m =
            match s with
            | Some v -> { p with errorMessageOpt = m + $" Message: {v}" |> ErrorMessage |> Some }
            | None ->   { p with errorMessageOpt = m |> ErrorMessage |> Some }

        match v with
        | AbortCalculation s -> $"The run queue was aborted at: %.2f{p.progress * 100.0m}%% progress." |> withMessage s
        | CancelWithResults s ->
            $"The run queue was cancelled at: %.2f{p.progress * 100.0m}%% progress. Absolute tolerance: {psd.nSolveParam.odeParams.absoluteTolerance}."
            |> withMessage s


    let mapResults psd (solverResult : SolverResult) (elapsed : TimeSpan) =
        let psd1 = { psd with tPrev = psd.t; t = solverResult.EndTime; x = solverResult.X }

        {
            startTime = solverResult.StartTime
            endTime = solverResult.EndTime
            xEnd = solverResult.X
            progressData = calculateProgressData psd1
        }


    let notifyProgress n s p =
        printDebug $"notifyProgress: Called with p = {p}."
//        Thread.Sleep(30_000)
        n.progressCallBack s p


    let checkCancellation psd =
        let fromLastCheck = DateTime.Now - psd.lastCheck
        printDebug $"checkCancellation: runQueueId = %A{n.runQueueId}, time interval from last check = %A{fromLastCheck}."

        if fromLastCheck > psd.nSolveParam.checkFreq
        then
            let psdNew = { psd with lastCheck = DateTime.Now }
            let cancel = psd.nSolveParam.checkCancellation psd.nSolveParam.runQueueId
            psdNew, cancel
        else psd, None


    let callBack psd =
        printDebug $"callBack: Called with t = {psd.t}."
        let psd1 = { psd with calculated = false }

        let g p =
            let p1 = { p with progress = calculateProgress psd; lastNotifiedT = psd.t }
            printProgressInfo p1
            p1

        match shouldNotifyProgress psd1, shouldNotifyChart psd1 with
        | true, true ->
            calculateProgressData psd1 |> notifyProgress psd1.nSolveParam None
            notifyChart psd1
            let psd2 = { psd1 with nextProgress = calculateNextProgress psd1; nextChartProgress = calculateNextChartProgress psd1 }
            g psd2
        | true, false ->
            calculateProgressData d |> notifyProgress n None
            nextProgress <- calculateNextProgress n d.t
            g()
        | false, true ->
            notifyChart d
            nextChartProgress <- calculateNextChartProgress n d.t
            g()
        | false, false -> psd1


    let needsCallBack n t =
        let r =
            callCount <- callCount + 1L
            checkCancellation n, shouldNotify n t

        printDebug $"needsCallBack: t = {t}, r = {r}."
        r


    let callBackUseNonNegative d =
        printDebug $"callBackUseNonNegative: Called with t = {d.t}."
        callCount <- callCount + 1L

        match checkCancellation d.nSolveParam with
        | Some v -> raise(ComputationAbortedException (calculateProgressDataWithErr d v, v))
        | None -> ()

        if shouldNotify d.nSolveParam d.t then callBack d


    let callBackDoNotCorrect c d =
        printDebug $"callBackDoNotCorrect: Called with t = {d.t}."

        match c with
        | Some v -> raise(ComputationAbortedException (calculateProgressDataWithErr d v, v))
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

            {
                startTime = p.startTime
                endTime = p.endTime
                xEnd = xEnd
                progressData = calculateProgressData { nSolveParam = n; t = p.endTime; x = xEnd }
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
                let callBack c t x = callBackDoNotCorrect c { nSolveParam = n; t = t; x = x }
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
