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

    let private makeNonNegative (x: double[]) = x |> Array.map (max 0.0)
    let private makeNonNegativeByRef (neq : int) (x : nativeptr<double>) : double[] = [| for i in 0.. neq - 1 -> max 0.0 (NativePtr.get x i) |]

    let private makeNonNegativeByRefInPlace (neq : int) (x : nativeptr<double>) =
        for i in 0 .. (neq - 1) do NativePtr.set x i (max 0.0 (NativePtr.get x i))


    let private fUseNonNegative (
                                derivativeCalculator : DerivativeCalculator,
                                callBack : CallBack,
                                neq : byref<int>,
                                t : byref<double>,
                                x : nativeptr<double>,
                                dx : nativeptr<double>) : unit =

        let x1 = makeNonNegativeByRef neq x
        callBack.invoke t x1

        match derivativeCalculator with
        | OneByOne f ->
            for i in 0 .. (neq - 1) do NativePtr.set dx i (f t x1 i)
        | FullArray f ->
            let d = f t x1
            for i in 0 .. (neq - 1) do NativePtr.set dx i d[i]


    // let calculateByRefDerivativeValue (x : nativeptr<double>) (indices : ModelIndices)  : double[] =
    //     failwith ""


    let private fDoNotCorrect (
                                // needsCallBack : double -> CancellationType option * bool,
                                // callBack : CancellationType option -> double -> double[] -> unit,
                                // derivativeCalculator : DerivativeCalculator,
                                derivativeCalculator : DerivativeCalculator,
                                callBack : CallBack,
                                needsCallBack : NeedsCallBackChecker,
                                neq : byref<int>,
                                t : byref<double>,
                                x : nativeptr<double>,
                                dx : nativeptr<double>) : unit =

        // match needsCallBack.invoke t with
        // | Some c, _ -> callBack (Some c) t (makeNonNegativeByRef neq x)
        // | None, true -> callBack None t (makeNonNegativeByRef neq x)
        // | None, false -> ()

        // let d = calculateDerivative t x
        // for i in 0 .. (neq - 1) do NativePtr.set dx i d[i]
        failwith "fDoNotCorrect is not implemented yet."


    let private createUseNonNegativeInterop (derivativeCalculator : DerivativeCalculator, callaBack : CallBack) =
        Interop.F(fun n t y dy -> fUseNonNegative(derivativeCalculator, callaBack, &n, &t, y, dy))


    let private createDoNotCorrectInterop (
                                    derivativeCalculator : DerivativeCalculator,
                                    callBack : CallBack,
                                    needsCallBack : NeedsCallBackChecker
                                    // needsCallBack: double -> CancellationType option * bool,
                                    // callaBack: CancellationType option -> double -> double[] -> unit,
                                    // calculateByRefDerivative : byref<double> -> nativeptr<double> -> double[]
                                    ) =
        Interop.F(fun n t y dy -> fDoNotCorrect(derivativeCalculator, callBack, needsCallBack, &n, &t, y, dy))


    // ================================================================ //

    //let private printDebug s = printfn $"{s}"
    let private printDebug _ = ()


    let private calculateProgress n t =
        (t - n.odeParams.startTime) / (n.odeParams.endTime - n.odeParams.startTime)
        |> decimal


    let private estCompl n t =
        match estimateEndTime (calculateProgress n t) n.started with
        | Some e -> " est. compl.: " + e.ToShortDateString() + ", " + e.ToShortTimeString() + ","
        | None -> EmptyString


    let private calculateProgressDataWithErr n t x v =
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
                tx = Some (t, x)
                data = 0
            }

        match v with
        | AbortCalculation s -> $"The run queue was aborted at: %.2f{progress * 100.0m}%% progress." |> withMessage s
        | CancelWithResults s ->
            $"The run queue was cancelled at: %.2f{progress * 100.0m}%% progress. Absolute tolerance: {n.odeParams.absoluteTolerance}."
            |> withMessage s


    let private checkCancellation n =
        let fromLastCheck = DateTime.Now - lastCheck
        printDebug $"checkCancellation: runQueueId = %A{n.runQueueId}, time interval from last check = %A{fromLastCheck}."

        if fromLastCheck > n.checkFreq
        then
            lastCheck <- DateTime.Now
            let cancel = n.checkCancellation.invoke n.runQueueId
            cancel
        else None

    // let callBack psd =
    //     // failwith "callBack not is not implemented yet."
    //     psd

    let private callBack n =
        let f t x =
            printDebug $"callBackUseNonNegative: Called with t = {t}."
            callCount <- callCount + 1L
            n.callBack.invoke t x

            match checkCancellation n with
            | Some v -> raise(ComputationAbortedException (calculateProgressDataWithErr n t x v, v))
            | None -> ()

        CallBack f

        // if shouldNotify psd then callBack psd else psd


    // let private callBackDoNotCorrect n t x =
    //     printDebug $"callBackDoNotCorrect: Called with t = {t}."
    //     callCount <- callCount + 1L
    //
    //     // match c with
    //     // | Some v -> raise(ComputationAbortedException ((calculateProgressDataWithErr psd v).lastProgressData, v))
    //     // | None -> ()
    //     //
    //     // if shouldNotify psd then callBack psd else psd
    //     failwith "callBackDoNotCorrect is not implemented yet."
    //     ()


    /// F# wrapper around various ODE solvers.
    let nSolve<'T> (n : NSolveParam<'T>) : OdeResult<'T> =
        printfn "nSolve::Starting."
        let p = n.odeParams
        let callBack = callBack n

        let mapResults (r : SolverResult) _ =
            {
                startTime = 0.0
                endTime = r.EndTime
                xEnd = r.X
                data = n.getData r.EndTime r.X
            }

        // let mutable psd = createProgressStateData n

        // let callBackUseNonNegative t x =
        //     // let a = callBackUseNonNegative { nSolveParam = n; t = t; x = x }
        //     // psd <- callBackUseNonNegative { psd with t = t; x = x }
        //     failwith "callBackUseNonNegative is not implemented yet."
        //     ()

        // let d = StatUpdateData.create n
        // let csd = n.getChartSliceData d.t d.x EeData.defaultValue
        // lastChartSliceData <- csd
        // dtEeSum <- csd.enantiomericExcess |> Array.map (fun _ -> 0.0)
        // tDtEeSum <- csd.enantiomericExcess |> Array.map (fun _ -> 0.0)
        // firstChartSliceData <- calculateChartSliceData d
        // calculateProgressData d |> notifyProgress n (Some InProgressRunQueue)

        match n.odeParams.solverType with
        | AlgLib CashCarp ->
            printfn "nSolve: Using Cash - Carp Alglib solver."
            let nt = 2

            let cashCarpDerivative (x : double[]) (t : double) : double[] =
                n.callBack.invoke t x
                n.derivative.calculate t x

            let x : array<double> = [| for i in 0..nt -> p.startTime + (p.endTime - p.startTime) * (double i) / (double nt) |]
            let d = alglib.ndimensional_ode_rp (fun x t y _ -> cashCarpDerivative x t |> Array.mapi(fun i e -> y[i] <- e) |> ignore)
            let mutable s = alglib.odesolverrkck(n.initialValues, x, p.absoluteTolerance.value, p.stepSize)
            do alglib.odesolversolve(s, d, null)
            let mutable m, xTbl, yTbl, rep = alglib.odesolverresults(s)
            let xEnd = yTbl[nt - 1, *]

            {
                startTime = p.startTime
                endTime = p.endTime
                xEnd = xEnd
                data = n.getData p.endTime xEnd
            }
        | OdePack (m, i, nc) ->
            printfn $"nSolve: Using {m} / {i} DLSODE solver."

            match nc with
            | UseNonNegative ->
                let interop() = createUseNonNegativeInterop(n.derivative, callBack)

                OdeSolver.RunFSharp(
                        (fun() -> interop()),
                        m.value,
                        i.value,
                        p.startTime,
                        p.endTime,
                        n.initialValues,
                        mapResults,
                        p.absoluteTolerance.value)

            | DoNotCorrect ->
                // let needsCallBack t =
                //     let a, b, c = needsCallBack psd t
                //     psd <- a
                //     (b, c)
                //
                // let callBack c t x =
                //     psd <- callBackDoNotCorrect c { psd with t = t; x = x }
                //     ()

                let interop() = createDoNotCorrectInterop(n.derivative, callBack, n.needsCallBack)

                OdeSolver.RunFSharp(
                        (fun() -> interop()),
                        m.value,
                        i.value,
                        p.startTime,
                        p.endTime,
                        n.initialValues,
                        mapResults,
                        p.absoluteTolerance.value)
