namespace GenericOdeSolver

open System
open Microsoft.FSharp.NativeInterop
open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives
open Primitives.SolverRunnerErrors
open Softellect.OdePackInterop
open Microsoft.FSharp.Core
open Primitives.GeneralData
open GenericOdeSolver.Primitives

#nowarn "9"

module Solver =

    let mutable private needsCallBackData = NeedsCallBackData.defaultValue

    // ================================================================ //

    let private makeNonNegativeByRef (neq : int) (x : nativeptr<double>) : double[] = [| for i in 0..(neq - 1) -> max 0.0 (NativePtr.get x i) |]
    let private toArray (neq : int) (x : nativeptr<double>) : double[] = [| for i in 0..(neq - 1) -> NativePtr.get x i |]


    let calculateProgress n t =
        (t - n.odeParams.startTime) / (n.odeParams.endTime - n.odeParams.startTime)
        |> decimal


    let shouldNotifyByCallCount d =
        let callCount = d.progressData.callCount

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

        // printDebug $"shouldNotifyByCallCount: callCount = {callCount}, r = {r}."
        r


    let shouldNotifyByNextProgress (n : NSolveParam) d t =
        let p = calculateProgress n t
        let r = p >= d.nextProgress
        // n.logger.logDebugString $"shouldNotifyByNextProgress: p = {p}, nextProgress = {d.nextProgress}, r = {r}."
        r


    let shouldNotifyByNextChartProgress (n : NSolveParam) d t =
        let p = calculateProgress n t
        let r = p >= d.nextChartProgress
        // n.logger.logDebugString $"shouldNotifyByNextChart: p = {p}, nextChartProgress = {d.nextChartProgress}, r = {r}."
        r


    let calculateNextProgress n t =
        let r =
            match n.odeParams.outputParams.noOfProgressPoints with
            | np when np <= 0 -> 1.0m
            | np -> min 1.0m ((((calculateProgress n t) * (decimal np) |> floor) + 1.0m) / (decimal np))
        // n.logger.logDebugString $"calculateNextProgress: r = {r}."
        r

    let calculateNextChartProgress n t =
        let r =
            match n.odeParams.outputParams.noOfOutputPoints with
            | np when np <= 0 -> 1.0m
            | np -> min 1.0m ((((calculateProgress n t) * (decimal np) |> floor) + 1.0m) / (decimal np))
        // n.logger.logDebugString $"calculateNextChartProgress: r = {r}."
        r

    let shouldNotifyProgress n d t = shouldNotifyByCallCount d || shouldNotifyByNextProgress n d t
    let shouldNotifyChart n d t = shouldNotifyByCallCount d || shouldNotifyByNextChartProgress n d t


    type OdeOutputParams
        with
        member op.needsCallBack n =
            let f (d : NeedsCallBackData) t =
                let shouldNotifyProgress = shouldNotifyProgress n d t
                let shouldNotifyChart = shouldNotifyChart n d t
                // n.logger.logDebugString $"needsCallBack: shouldNotifyProgress = {shouldNotifyProgress}, shouldNotifyChart = {shouldNotifyChart}."

                let retVal =
                    match (shouldNotifyProgress, shouldNotifyChart) with
                    | false, false -> (d, None)
                    | false, true -> ( { d with nextChartProgress = calculateNextChartProgress n t }, Some ChartNotification)
                    | true, false -> ( { d with nextProgress = calculateNextProgress n t }, Some ProgressNotification)
                    | true, true ->
                        let nextProgress = calculateNextProgress n t
                        let nextChartProgress = calculateNextChartProgress n t
                        ( { d with nextProgress = nextProgress; nextChartProgress = nextChartProgress }, Some ProgressAndChartNotification)

                // n.logger.logDebugString $"needsCallBack: retVal = {retVal}."
                retVal

            NeedsCallBack f


    let private checkCancellation n d =
        let fromLastCheck = DateTime.Now - d.lastCheck
        // n.logger.logDebugString $"checkCancellation: runQueueId = %A{n.runQueueId}, time interval from last check = %A{fromLastCheck}."

        if fromLastCheck > n.callBackInfo.checkFreq
        then
            let cancel = n.callBackInfo.checkCancellation.invoke n.runQueueId
            { d with lastCheck = DateTime.Now}, cancel
        else d, None


    let private estCompl n t =
        match estimateEndTime (calculateProgress n t) n.started with
        | Some e -> " est. compl.: " + e.ToShortDateString() + ", " + e.ToShortTimeString() + ","
        | None -> EmptyString


    let private calculateProgressDataWithErr n (d : NeedsCallBackData) t v =
        // n.logger.logDebugString $"calculateProgressDataWithErr: Called with t = {t}, v = {v}."

        let withMessage s m =
            let eo =
                match s with
                | Some v -> m + $" Message: {v}"
                | None -> m
                |> ErrorMessage
                |> Some

            let pd =
                {
                    progress = d.progressData.progress
                    callCount = d.progressData.callCount
                    errorMessageOpt = eo
                }

            pd

        match v with
        | AbortCalculation s -> $"The run queue was aborted at: %.2f{d.progressData.progress * 100.0m}%% progress." |> withMessage s
        | CancelWithResults s ->
            $"The run queue was cancelled at: %.2f{d.progressData.progress * 100.0m}%% progress. Absolute tolerance: {n.odeParams.absoluteTolerance}."
            |> withMessage s


    let private notifyAll n c d =
        n.callBackInfo.progressCallBack.invoke c d
        n.callBackInfo.chartCallBack.invoke c d


    let private tryCallBack n t x =
        let d0 = needsCallBackData
        let d, ct = { d0 with progressData = { d0.progressData with callCount = d0.progressData.callCount + 1L; progress = calculateProgress n t } } |> checkCancellation n
        let cbd = { progressData = d.progressData; t = t; x = x }
        // n.logger.logDebugString $"tryCallBack: d = {d}, cbd = {cbd}."

        match ct with
        | Some v ->
            notifyAll n (v |> CancelledCalculation |> FinalCallBack) cbd
            raise(ComputationAbortedException (calculateProgressDataWithErr n d t v, v))
        | None ->
            // let c, v = n.callBackInfo.needsCallBack.invoke d t
            let c, v = (n.odeParams.outputParams.needsCallBack n).invoke d t
            // n.logger.logDebugString $"tryCallBack: c = {c}, v = {v}."
            needsCallBackData <- c

            match v with
            | None -> ()
            | Some v ->
                match v with
                | ProgressNotification -> n.callBackInfo.progressCallBack.invoke RegularCallBack cbd
                | ChartNotification -> n.callBackInfo.chartCallBack.invoke RegularCallBack cbd
                | ProgressAndChartNotification -> notifyAll n RegularCallBack cbd


    let private fUseNonNegative (
                                nSolveParam : NSolveParam,
                                neq : byref<int>,
                                t : byref<double>,
                                x : nativeptr<double>,
                                dx : nativeptr<double>) : unit =

        let x1 = makeNonNegativeByRef neq x
        tryCallBack nSolveParam t x1

        match nSolveParam.derivative with
        | OneByOne f -> for i in 0..(neq - 1) do NativePtr.set dx i (f t x1 i)
        | FullArray f ->
            let d = f t x1
            for i in 0..(neq - 1) do NativePtr.set dx i d[i]


    let private fDoNotCorrect (
                                nSolveParam : NSolveParam,
                                neq : byref<int>,
                                t : byref<double>,
                                x : nativeptr<double>,
                                dx : nativeptr<double>) : unit =

        // if needsCallBack.invoke t
        // then
        //     let x1 = toArray neq x
        //     callBack.invoke t x1
        //
        // let d = calculateDerivative t x
        // for i in 0 .. (neq - 1) do NativePtr.set dx i d[i]
        failwith "fDoNotCorrect is not implemented yet."


    let private createUseNonNegativeInterop n = Interop.F(fun m t y dy -> fUseNonNegative(n, &m, &t, y, dy))
    let private createDoNotCorrectInterop n = Interop.F(fun m t y dy -> fDoNotCorrect(n, &m, &t, y, dy))


    /// F# wrapper around various ODE solvers.
    let nSolve (n : NSolveParam) =
        // n.logger.logDebugString "nSolve::Starting."
        let p = n.odeParams
        notifyAll n RegularCallBack { progressData = ProgressData.defaultValue; t = n.odeParams.startTime; x = n.initialValues }

        let mapResults (r : SolverResult) _ =
            {
                progressData = needsCallBackData.progressData
                t = r.EndTime
                x = r.X
            }

        match n.odeParams.solverType with
        | AlgLib CashCarp ->
            n.logger.logDebugString "nSolve: Using Cash - Carp Alglib solver."
            let nt = 2

            let cashCarpDerivative (x : double[]) (t : double) : double[] =
                tryCallBack n t x
                n.derivative.calculate t x

            let x : array<double> = [| for i in 0..nt -> p.startTime + (p.endTime - p.startTime) * (double i) / (double nt) |]
            let d = alglib.ndimensional_ode_rp (fun x t y _ -> cashCarpDerivative x t |> Array.mapi(fun i e -> y[i] <- e) |> ignore)
            let mutable s = alglib.odesolverrkck(n.initialValues, x, p.absoluteTolerance.value, p.stepSize)
            do alglib.odesolversolve(s, d, null)
            let mutable m, xTbl, yTbl, rep = alglib.odesolverresults(s)
            let xEnd = yTbl[nt - 1, *]
            notifyAll n (FinalCallBack CompletedCalculation) { progressData = needsCallBackData.progressData; t = p.endTime; x = xEnd }

            {
                progressData = needsCallBackData.progressData
                t = p.endTime
                x = xEnd
            }

        | OdePack (m, i, nc) ->
            n.logger.logDebugString $"nSolve: Using {m} / {i} / {nc} DLSODE solver."

            let result =
                match nc with
                | UseNonNegative ->
                    OdeSolver.RunFSharp(
                            (fun() -> createUseNonNegativeInterop n),
                            m.value,
                            i.value,
                            p.startTime,
                            p.endTime,
                            n.initialValues,
                            mapResults,
                            p.absoluteTolerance.value)

                | DoNotCorrect ->
                    OdeSolver.RunFSharp(
                            (fun() -> createDoNotCorrectInterop n),
                            m.value,
                            i.value,
                            p.startTime,
                            p.endTime,
                            n.initialValues,
                            mapResults,
                            p.absoluteTolerance.value)

            notifyAll n (FinalCallBack CompletedCalculation) result
            result
