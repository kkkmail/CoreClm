namespace OdeSolver

open System
open System.Threading
open Microsoft.FSharp.NativeInterop
open Softellect.OdePackInterop
open Softellect.OdePackInterop.Sets
open Softellect.OdePackInterop.SolverDescriptors
open Microsoft.FSharp.Core
open Clm.ChartData
open System
open ClmSys.ContGenPrimitives
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
            noOfOutputPoints : int option
            noOfProgressPoints : int option
        }

        static member defaultValue startTime endTime op pp =
            {
                startTime = startTime
                endTime = endTime
                stepSize = 0.1
                eps = 0.000_01
                noOfOutputPoints = op |> Option.defaultValue defaultNoOfOutputPoints |> Some
                noOfProgressPoints = pp |> Option.defaultValue defaultNoOfProgressPoints |> Some
            }


    type OdeResult =
        {
            startTime : double
            endTime : double
            xEnd : double[]
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
        | UseNonNegative
        | DoNotCorrect


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
            progressCallBack : (ProgressData -> unit) option
            chartCallBack : (ChartSliceData -> unit) option
            getChartSliceData : double -> double[] -> ChartSliceData
            noOfOutputPoints : int option
            noOfProgressPoints : int option
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


    let mapResults (solverResult : SolverResult) (elapsed : TimeSpan) =
        {
            startTime = solverResult.StartTime
            endTime = solverResult.EndTime
            xEnd = solverResult.X
        }


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


    let shouldNotifyByNextProgress (n : NSolveParam) t =
        let p = calculateProgress n t
        p >= nextProgress


    let calculateNextProgress n t =
        match n.noOfProgressPoints with
        | Some np -> min 1.0 ((((calculateProgress n t) * (double np) |> floor) + 1.0) / (double np))
        | None -> 1.0


    let shouldNotifyChart n t =
        match n.chartCallBack, n.noOfOutputPoints, n.noOfChartDetailedPoints with
        | Some _, Some np, Some cp ->
            ((double np) * nextProgress <= double cp && shouldNotifyByCallCount()) || shouldNotifyByNextProgress n t
        | _ -> false


    let shouldNotifyProgress n t =
        match n.progressCallBack with
        | Some _ -> shouldNotifyByCallCount() || shouldNotifyByNextProgress n t
        | None -> false


    let shouldNotify n t = shouldNotifyProgress n t || shouldNotifyChart n t


    let calculateChartSliceData n t x =
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
        match n.chartCallBack with
        | Some c -> calculateChartSliceData n t x |> c
        | None -> ()


    let calculateProgressData n t x =
        let csd = calculateChartSliceData n t x

        {
            progress = calculateProgress n t
            callCount = callCount
            eeData = lastEeData
            yRelative = csd.totalSubst.totalData / firstChartSliceData.totalSubst.totalData
            errorMessageOpt = None
        }


    let notifyProgress n p =
//        Thread.Sleep(30_000)
        match n.progressCallBack with
        | Some c -> c p
        | None -> ()


    /// kk:20200410 - Note that we have to resort to using exceptions for flow control here.
    /// There seems to be no other easy and clean way. Revisit if that changes.
    /// Follow the trail of that date stamp to find other related places.
    let checkCancellation n =
        let fromLastCheck = DateTime.Now - lastCheck
        //printfn "checkCancellation: runQueueId = %A, time interval from last check = %A." n.runQueueId fromLastCheck

        if fromLastCheck > n.checkFreq
        then
            lastCheck <- DateTime.Now
            let cancel = n.checkCancellation n.runQueueId

            match cancel with
            | Some c -> raise(ComputationAbortedException (n.runQueueId, c))
            | None -> ()


    let callBack n (t : double) (x : double[]) : unit =
        calculated <- false

        if shouldNotifyProgress n t
        then
            calculateProgressData n t x |> notifyProgress n
            nextProgress <- calculateNextProgress n t

        if shouldNotifyChart n t then notifyChart n t x


    let needsCallBack n t =
        callCount <- callCount + 1L
        checkCancellation n
        shouldNotify n t


    let callBackFunctional n t x =
        callCount <- callCount + 1L
        checkCancellation n
        if shouldNotify n t then callBack n t x


    let callBackChordWithDiagonalJacobian n t x = if shouldNotify n t then callBack n t x


    /// F# wrapper around various ODE solvers.
    let nSolve (n : NSolveParam) : OdeResult =
        printfn "nSolve::Starting."
        let p = OdeParams.defaultValue n.tStart n.tEnd n.noOfOutputPoints n.noOfProgressPoints
        let callBackFunctional t x = callBackFunctional n t x
        firstChartSliceData <- calculateChartSliceData n 0.0 n.initialValues

        calculateProgressData n n.tStart n.initialValues |> notifyProgress n

        match n.solverType with
        | AlgLib CashCarp ->
            printfn "nSolve: Using Cash - Carp Alglib solver."
            let nt = 2
            let cashCarpDerivative (x : double[]) (t : double) : double[] = n.calculationData.getDerivative x
            let x : array<double> = [| for i in 0..nt -> p.startTime + (p.endTime - p.startTime) * (double i) / (double nt) |]
            let d = alglib.ndimensional_ode_rp (fun x t y _ -> cashCarpDerivative x t |> Array.mapi(fun i e -> y.[i] <- e) |> ignore)
            let mutable s = alglib.odesolverrkck(n.initialValues, x, p.eps, p.stepSize)
            do alglib.odesolversolve(s, d, null)
            let mutable (m, xTbl, yTbl, rep) = alglib.odesolverresults(s)

            {
                startTime = p.startTime
                endTime = p.endTime
                xEnd = yTbl.[nt - 1, *]
            }
        | OdePack (m, i, nc) ->
            printfn $"nSolve: Using {m} / {i} DLSODE solver."
            match i with
            | Functional ->
                let interop() = createInterop(callBackFunctional, n.calculationData.modelIndices)
                // TODO kk:20210316 - UseNonNegative is hardcoded below.
                OdeSolver.RunFSharp((fun() -> interop()), m.value, i.value, p.startTime, p.endTime, n.initialValues, (fun r e -> mapResults r e))
            | ChordWithDiagonalJacobian ->
                let needsCallBack t = needsCallBack n t
                let callBack t x = callBackChordWithDiagonalJacobian n t x
                let interop() = createInterop1(needsCallBack, callBack, n.calculationData.modelIndices)
                // TODO kk:20210316 - DoNotCorrect is hardcoded below.
                OdeSolver.RunFSharp((fun() -> interop()), m.value, i.value, p.startTime, p.endTime, n.initialValues, (fun r e -> mapResults r e))
