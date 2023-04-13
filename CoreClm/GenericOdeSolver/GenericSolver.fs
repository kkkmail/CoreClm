namespace GenericOdeSolver

open System
// open ClmSys.ContGenPrimitives
open Microsoft.FSharp.NativeInterop
open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives
open Primitives.SolverRunnerErrors
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

#nowarn "9"

module Solver =

    type DerivativeCalculator =
        | OneByOne of (double -> double[] -> int -> double)
        | FullArray of (double -> double[] -> double[])

        member d.calculateArray t x =
            match d with
            | OneByOne f -> x |> Array.mapi (fun i _ -> f t x i)
            | FullArray f -> f t x


    let makeNonNegative (x: double[]) = x |> Array.map (max 0.0)
    let makeNonNegativeByRef (neq : int) (x : nativeptr<double>) : double[] = [| for i in 0.. neq - 1 -> max 0.0 (NativePtr.get x i) |]

    let makeNonNegativeByRefInPlace (neq : int) (x : nativeptr<double>) =
        for i in 0 .. (neq - 1) do NativePtr.set x i (max 0.0 (NativePtr.get x i))


    let private fUseNonNegative (
                                callBack : double -> double[] -> unit,
                                derivativeCalculator : DerivativeCalculator,
                                neq : byref<int>,
                                t : byref<double>,
                                x : nativeptr<double>,
                                dx : nativeptr<double>) : unit =

        let x1 = makeNonNegativeByRef neq x
        callBack t x1

        match derivativeCalculator with
        | OneByOne f ->
            for i in 0 .. (neq - 1) do NativePtr.set dx i (f t x1 i)
        | FullArray f ->
            let d = f t x1
            for i in 0 .. (neq - 1) do NativePtr.set dx i d[i]


    // let calculateByRefDerivativeValue (x : nativeptr<double>) (indices : ModelIndices)  : double[] =
    //     failwith ""


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

        // let d = calculateDerivative t x
        // for i in 0 .. (neq - 1) do NativePtr.set dx i d[i]
        failwith "fDoNotCorrect is not implemented yet."


    let createUseNonNegativeInterop (callaBack: double -> double[] -> unit, derivativeCalculator : DerivativeCalculator) =
        Interop.F(fun n t y dy -> fUseNonNegative(callaBack, derivativeCalculator, &n, &t, y, dy))


    let createDoNotCorrectInterop (
                                    needsCallBack: double -> CancellationType option * bool,
                                    callaBack: CancellationType option -> double -> double[] -> unit,
                                    derivativeCalculator : DerivativeCalculator
                                    // calculateByRefDerivative : byref<double> -> nativeptr<double> -> double[]
                                    ) =
        Interop.F(fun n t y dy -> fDoNotCorrect(needsCallBack, callaBack, derivativeCalculator, &n, &t, y, dy))


    // ================================================================ //

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


    type NSolveData =
        {
            odeParams : OdeParams
            solverType : SolverType
            modelDataId : Guid
            runQueueId : RunQueueId
            initialValues : double[]
            getDerivative : DerivativeCalculator
            checkFreq : TimeSpan
            checkCancellation : RunQueueId -> CancellationType option
        }

    type NSolveParam<'PD, 'CSD, 'ED> =
        {
            nSolveData : NSolveData

            // getByRefDerivative : byref<double> -> nativeptr<double> -> double[]
            defaultProgressData : ProgressData<'PD>
            defaultChartSliceData : 'CSD
            defaultExtraData : 'ED

            progressCallBack : RunQueueStatus option -> ProgressData<'PD> -> unit
            chartCallBack : 'CSD -> unit
            getChartSliceData : double -> double[] -> 'PD -> 'CSD
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


    type private ProgressStateData<'PD, 'CSD, 'ED> =
        {
            nSolveParam : NSolveParam<'PD, 'CSD, 'ED>
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
            lastProgressData : ProgressData<'PD>
            extraData : 'ED

            tPrev : double
            tDtSum : double
            calculated : bool
        }

    let private createProgressStateData n =
        {
            nSolveParam = n
            t = 0.0
            x =  n.nSolveData.initialValues

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



    let private calculateProgress psd =
        (psd.t - psd.nSolveParam.nSolveData.odeParams.startTime) / (psd.nSolveParam.nSolveData.odeParams.endTime - psd.nSolveParam.nSolveData.odeParams.startTime)
        |> decimal


    let private estCompl psd =
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


    let private printProgressInfo psd =
        printfn $"printProgressInfo: t = {psd.t}, progress = {psd.progress}, lastProgressData = %0A{psd.lastProgressData}."


    let private shouldNotifyByCallCount psd =
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


    let private shouldNotifyByNextProgress psd =
        let p = calculateProgress psd
        let r = p >= psd.nextProgress
        printDebug $"shouldNotifyByNextProgress: p = {p}, nextProgress = {psd.nextProgress}, r = {r}."
        r


    let private shouldNotifyByNextChartProgress psd =
        let p = calculateProgress psd
        let r = p >= psd.nextChartProgress
        printDebug $"shouldNotifyByNextChart: p = {p}, nextChartProgress = {psd.nextChartProgress}, r = {r}."
        r


    let private calculateNextProgress psd =
        let r =
            match psd.nSolveParam.nSolveData.odeParams.noOfProgressPoints with
            | np when np <= 0 -> 1.0m
            | np -> min 1.0m ((((calculateProgress psd) * (decimal np) |> floor) + 1.0m) / (decimal np))

        printDebug $"calculateNextProgress: r = {r}."
        r

    let private calculateNextChartProgress psd =
        let r =
            match psd.nSolveParam.nSolveData.odeParams.noOfOutputPoints with
            | np when np <= 0 -> 1.0m
            | np -> min 1.0m ((((calculateProgress psd) * (decimal np) |> floor) + 1.0m) / (decimal np))

        printDebug $"calculateNextChartProgress: r = {r}."
        r

    let private shouldNotifyProgress psd = shouldNotifyByCallCount psd || shouldNotifyByNextProgress psd
    let private shouldNotifyChart psd = shouldNotifyByCallCount psd || shouldNotifyByNextChartProgress psd


    /// Don't notify twice for the same value of t. This could happen during diagonal Jacobian evaluation.
    let private shouldNotify psd = (shouldNotifyProgress psd || shouldNotifyChart psd) && (psd.lastNotifiedT <> psd.t)


    let private calculateChartSliceData psd =
        let n = psd.nSolveParam
        printDebug $"calculateChartSliceData: Called with t = {psd.t}."

        if psd.calculated
        then psd.lastChartSliceData
        else
            let dt = psd.t - psd.tPrev
            let tDt = psd.t * dt
            let csd = n.getChartSliceData psd.t psd.x n.defaultProgressData.progressData

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
            // psdNew
            failwith ""


    let private notifyChart psd =
//        Thread.Sleep(30_000)
        printDebug $"notifyChart: Calling chartCallBack with t = {psd.t}."
        let psdNew = calculateChartSliceData psd
        psd.lastChartSliceData |> psd.nSolveParam.chartCallBack


    let private calculateProgressData psd =
        printDebug $"calculateProgressData: Called with t = {psd.t}."
        let psdNew =
            {
                psd
                with
                    lastChartSliceData = calculateChartSliceData psd
                    lastProgressData = failwith ""
            }


        // {
        //     progress = calculateProgress psd
        //     callCount = psd.callCount
        //     progressData = psd.lastProgressData
        //     yRelative = csd.totalSubst.totalData / firstChartSliceData.totalSubst.totalData
        //     errorMessageOpt = None
        // }
        psdNew


    let private calculateProgressDataWithErr psd v =
        printDebug $"calculateProgressDataWithErr: Called with t = {psd.t}, v = {v}."
        let p = calculateProgressData psd

        let withMessage s m =
            let eo =
                match s with
                | Some v -> m + $" Message: {v}"
                | None -> m
                |> ErrorMessage
                |> Some

            { psd with lastProgressData = { psd.lastProgressData with errorMessageOpt = eo } }

        match v with
        | AbortCalculation s -> $"The run queue was aborted at: %.2f{p.progress * 100.0m}%% progress." |> withMessage s
        | CancelWithResults s ->
            $"The run queue was cancelled at: %.2f{p.progress * 100.0m}%% progress. Absolute tolerance: {psd.nSolveParam.nSolveData.odeParams.absoluteTolerance}."
            |> withMessage s


    let private mapResults psd (solverResult : SolverResult) (elapsed : TimeSpan) =
        let psd1 = { psd with tPrev = psd.t; t = solverResult.EndTime; x = solverResult.X }

        {
            startTime = solverResult.StartTime
            endTime = solverResult.EndTime
            xEnd = solverResult.X
            progressData = (calculateProgressData psd1).lastProgressData
        }


    let private notifyProgress psd s =
        printDebug $"notifyProgress: Called with p = {psd.lastProgressData}."
//        Thread.Sleep(30_000)
        psd.nSolveParam.progressCallBack s psd.lastProgressData


    let private checkCancellation psd =
        let fromLastCheck = DateTime.Now - psd.lastCheck
        printDebug $"checkCancellation: runQueueId = %A{psd.nSolveParam.nSolveData.runQueueId}, time interval from last check = %A{fromLastCheck}."

        if fromLastCheck > psd.nSolveParam.nSolveData.checkFreq
        then
            let psdNew = { psd with lastCheck = DateTime.Now }
            let cancel = psd.nSolveParam.nSolveData.checkCancellation psd.nSolveParam.nSolveData.runQueueId
            psdNew, cancel
        else psd, None

    let callBack psd =
        failwith "callBack psd is nto implemented yet."
        psd

    // let callBack psd =
    //     printDebug $"callBack: Called with t = {psd.t}."
    //     let psd1 = { psd with calculated = false }
    //
    //     let g p =
    //         let p1 = { p with progress = calculateProgress psd; lastNotifiedT = psd.t }
    //         printProgressInfo p1
    //         p1
    //
    //     match shouldNotifyProgress psd1, shouldNotifyChart psd1 with
    //     | true, true ->
    //         calculateProgressData psd1 |> notifyProgress psd1 None
    //         notifyChart psd1
    //         let psd2 = { psd1 with nextProgress = calculateNextProgress psd1; nextChartProgress = calculateNextChartProgress psd1 }
    //         g psd2
    //     | true, false ->
    //         let psd2 = calculateProgressData psd1
    //         notifyProgress n None
    //         let psd2 = { psd1 with nextProgress = calculateNextProgress psd1 }
    //         g psd2
    //     | false, true ->
    //         notifyChart d
    //         nextChartProgress <- calculateNextChartProgress n d.t
    //         g()
    //     | false, false -> psd1
    //
    //
    let private needsCallBack psd t =
        let r =
            // callCount <- callCount + 1L
            let a, b = checkCancellation psd
            a, b, shouldNotify psd

        printDebug $"needsCallBack: t = {t}, r = {r}."
        r


    let private callBackUseNonNegative<'PD, 'CSD, 'ED> (psd : ProgressStateData<'PD, 'CSD, 'ED>) =
        printDebug $"callBackUseNonNegative: Called with t = {psd.t}."
        // callCount <- callCount + 1L

        match checkCancellation psd with
        | _, Some v -> raise(ComputationAbortedException<'PD> ((calculateProgressDataWithErr psd v).lastProgressData, v))
        | _, None -> ()

        if shouldNotify psd then callBack psd else psd


    let private callBackDoNotCorrect<'PD> c psd =
        printDebug $"callBackDoNotCorrect: Called with t = {psd.t}."

        match c with
        | Some v -> raise(ComputationAbortedException<'PD> ((calculateProgressDataWithErr psd v).lastProgressData, v))
        | None -> ()

        if shouldNotify psd then callBack psd else psd


    /// F# wrapper around various ODE solvers.
    let nSolve n = //(n : NSolveParam) : OdeResult =
        printfn "nSolve::Starting."
        let p = n.nSolveData.odeParams
        let mutable psd = createProgressStateData n

        let callBackUseNonNegative t x =
            // let a = callBackUseNonNegative { nSolveParam = n; t = t; x = x }
            psd <- callBackUseNonNegative { psd with t = t; x = x }
            ()

        // let d = StatUpdateData.create n
        // let csd = n.getChartSliceData d.t d.x EeData.defaultValue
        // lastChartSliceData <- csd
        // dtEeSum <- csd.enantiomericExcess |> Array.map (fun _ -> 0.0)
        // tDtEeSum <- csd.enantiomericExcess |> Array.map (fun _ -> 0.0)
        // firstChartSliceData <- calculateChartSliceData d
        // calculateProgressData d |> notifyProgress n (Some InProgressRunQueue)

        match n.nSolveData.solverType with
        | AlgLib CashCarp ->
            printfn "nSolve: Using Cash - Carp Alglib solver."
            let nt = 2

            let cashCarpDerivative (x : double[]) (t : double) : double[] =
                callBackUseNonNegative t x
                n.nSolveData.getDerivative.calculateArray t x

            let x : array<double> = [| for i in 0..nt -> p.startTime + (p.endTime - p.startTime) * (double i) / (double nt) |]
            let d = alglib.ndimensional_ode_rp (fun x t y _ -> cashCarpDerivative x t |> Array.mapi(fun i e -> y[i] <- e) |> ignore)
            let mutable s = alglib.odesolverrkck(n.nSolveData.initialValues, x, p.absoluteTolerance.value, p.stepSize)
            do alglib.odesolversolve(s, d, null)
            let mutable (m, xTbl, yTbl, rep) = alglib.odesolverresults(s)
            let xEnd = yTbl[nt - 1, *]
            psd <- calculateProgressData { psd with t = p.endTime; x = xEnd }

            {
                startTime = p.startTime
                endTime = p.endTime
                xEnd = xEnd
                progressData = psd.lastProgressData
            }
        | OdePack (m, i, nc) ->
            printfn $"nSolve: Using {m} / {i} DLSODE solver."

            match nc with
            | UseNonNegative ->
                let interop() = createUseNonNegativeInterop(callBackUseNonNegative, n.nSolveData.getDerivative)

                OdeSolver.RunFSharp(
                        (fun() -> interop()),
                        m.value,
                        i.value,
                        p.startTime,
                        p.endTime,
                        n.nSolveData.initialValues,
                        (fun r e -> mapResults psd r e),
                        p.absoluteTolerance.value)

            | DoNotCorrect ->
                let needsCallBack t =
                    let a, b, c = needsCallBack psd t
                    psd <- a
                    (b, c)

                let callBack c t x =
                    psd <- callBackDoNotCorrect c { psd with t = t; x = x }
                    ()

                let interop() = createDoNotCorrectInterop(needsCallBack, callBack, n.nSolveData.getDerivative)

                OdeSolver.RunFSharp(
                        (fun() -> interop()),
                        m.value,
                        i.value,
                        p.startTime,
                        p.endTime,
                        n.nSolveData.initialValues,
                        (fun r e -> mapResults psd r e),
                        p.absoluteTolerance.value)
