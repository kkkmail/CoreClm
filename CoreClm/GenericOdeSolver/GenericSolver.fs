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

    let mutable private callCount = 0L
    let mutable private progress = 0.0m
    let mutable private lastCheck = DateTime.Now
    let mutable private callBackData = CallBackData.defaultValue

    // ================================================================ //

    //let private printDebug s = printfn $"{s}"
    let private printDebug _ = ()
    let private makeNonNegativeByRef (neq : int) (x : nativeptr<double>) : double[] = [| for i in 0..(neq - 1) -> max 0.0 (NativePtr.get x i) |]
    let private toArray (neq : int) (x : nativeptr<double>) : double[] = [| for i in 0..(neq - 1) -> NativePtr.get x i |]


    let private checkCancellation n =
        let fromLastCheck = DateTime.Now - lastCheck
        printDebug $"checkCancellation: runQueueId = %A{n.runQueueId}, time interval from last check = %A{fromLastCheck}."

        if fromLastCheck > n.callBackInfo.checkFreq
        then
            lastCheck <- DateTime.Now
            let cancel = n.callBackInfo.checkCancellation.invoke n.runQueueId
            cancel
        else None


    let private calculateProgress n t =
        (t - n.odeParams.startTime) / (n.odeParams.endTime - n.odeParams.startTime)
        |> decimal


    let private estCompl n t =
        match estimateEndTime (calculateProgress n t) n.started with
        | Some e -> " est. compl.: " + e.ToShortDateString() + ", " + e.ToShortTimeString() + ","
        | None -> EmptyString


    let private calculateProgressDataWithErr n t v =
        printDebug $"calculateProgressDataWithErr: Called with t = {t}, v = {v}."
        progress <- calculateProgress n t

        let withMessage s m =
            let eo =
                match s with
                | Some v -> m + $" Message: {v}"
                | None -> m
                |> ErrorMessage
                |> Some

            {
                progress = progress
                callCount = callCount
                errorMessageOpt = eo
            }

        match v with
        | AbortCalculation s -> $"The run queue was aborted at: %.2f{progress * 100.0m}%% progress." |> withMessage s
        | CancelWithResults s ->
            $"The run queue was cancelled at: %.2f{progress * 100.0m}%% progress. Absolute tolerance: {n.odeParams.absoluteTolerance}."
            |> withMessage s


    let private notifyAll n c t x =
        n.callBackInfo.progressCallBack.invoke c t x
        n.callBackInfo.chartCallBack.invoke c t x


    let private tryCallBack n t x =
        callCount <- callCount + 1L

        match checkCancellation n with
        | Some v ->
            notifyAll n (v |> CancelledCalculation |> FinalCallBack) t x
            raise(ComputationAbortedException (calculateProgressDataWithErr n t v, v))
        | None ->
            let c, v = n.callBackInfo.needsCallBack.invoke callBackData t
            callBackData <- c

            match v with
            | None -> ()
            | Some v ->
                match v with
                | ProgressNotification -> n.callBackInfo.progressCallBack.invoke RegularCallBack t x
                | ChartNotification -> n.callBackInfo.chartCallBack.invoke RegularCallBack t x
                | ProgressAndChartNotification -> notifyAll n RegularCallBack t x


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
    let nSolve (n : NSolveParam) : OdeResult =
        printfn "nSolve::Starting."
        let p = n.odeParams
        notifyAll n RegularCallBack n.odeParams.startTime n.initialValues

        let mapResults (r : SolverResult) _ =
            {
                startTime = n.odeParams.startTime
                endTime = r.EndTime
                xEnd = r.X
            }

        match n.odeParams.solverType with
        | AlgLib CashCarp ->
            printfn "nSolve: Using Cash - Carp Alglib solver."
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
            notifyAll n (FinalCallBack CompletedCalculation) p.endTime xEnd

            {
                startTime = p.startTime
                endTime = p.endTime
                xEnd = xEnd
            }

        | OdePack (m, i, nc) ->
            printfn $"nSolve: Using {m} / {i} DLSODE solver."

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
