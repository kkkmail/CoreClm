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
            progressCallBack : (decimal -> UnitResult) option
            chartCallBack : (ChartSliceData -> unit) option
            timeCallBack : (TimeData -> UnitResult) option
            getChartSliceData : double -> double[] -> ChartSliceData
            noOfOutputPoints : int option
            noOfProgressPoints : int option
            checkCancellation : RunQueueId -> CancellationType option
            checkFreq : TimeSpan
            timeCheckFreq : TimeSpan
        }

        member p.next tEndNew initValNew = { p with tStart = p.tEnd; tEnd = tEndNew; initialValues = initValNew }


    let calculateProgress r m = (decimal (max 0 (r - 1))) / (decimal m)


    let estCompl s r m =
        match estimateEndTime (calculateProgress r m) s with
        | Some e -> " est. compl.: " + e.ToShortDateString() + ", " + e.ToShortTimeString() + ","
        | None -> EmptyString


    let mapResults (solverResult : SolverResult) (elapsed : TimeSpan) =
        {
            startTime = solverResult.StartTime
            endTime = solverResult.EndTime
            xEnd = solverResult.X
        }


    let shouldNotify callCount =
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


    /// F# wrapper around Alglib ODE solver.
    let nSolve (n : NSolveParam) : OdeResult =
        printfn "nSolve::Starting."
        let mutable progressCount = 0
        let mutable outputCount = 0
        let mutable callCount = 0L
        let mutable lastCheck = DateTime.Now
        let mutable lastTimeCheck = lastCheck
        let mutable lastChartSliceData = ChartSliceData.defaultValue
        let mutable lastEeData = EeData.defaultValue
        let mutable tSum = 0.0
        let mutable eeCount = 0
        let mutable calculated = false
        let p = OdeParams.defaultValue n.tStart n.tEnd n.noOfOutputPoints n.noOfProgressPoints
        let shouldNotify() = shouldNotify callCount

        let calculateChartSliceData t x =
            if calculated then lastChartSliceData
            else
                let csd = n.getChartSliceData t x

                let eeData =
                    {
                        maxEe = max lastEeData.maxEe csd.maxEe
                        maxAverageEe = (lastEeData.maxAverageEe * (double eeCount) + csd.maxEe) / ((double eeCount) + 1.0)
                        maxWeightedAverageAbsEe = if t > 0.0 then (lastEeData.maxWeightedAverageAbsEe * tSum + csd.maxEe * t) / (tSum + t) else 0.0
                        maxLastEe = csd.maxEe
                    }

                tSum <- tSum + t
                eeCount <- eeCount + 1
                lastEeData <- eeData
                lastChartSliceData <- csd
                calculated <- true
                csd

        let notifyProgressDetailed p =
            match n.progressCallBack with
            | Some c -> c p
            | None -> Ok()

        let notifyProgress t r m =
//            Thread.Sleep(30_000)
            calculateProgress r m |> notifyProgressDetailed

        let notifyChart t x =
//            Thread.Sleep(30_000)
            match n.chartCallBack with
            | Some c -> calculateChartSliceData t x |> c
            | None -> ()

        let notifyTime t x force =
            match n.timeCallBack with
            | Some c ->
                let fromLastTimeCheck = DateTime.Now - lastTimeCheck

                if fromLastTimeCheck > n.timeCheckFreq || force
                then
                    lastTimeCheck <- DateTime.Now
                    let csd = calculateChartSliceData t x

                    let td =
                        {
                            progressDetailed = t / (n.tEnd - n.tStart)
                            callCount = callCount
                            eeData = lastEeData
                            y = csd.totalSubst.totalData
                        }

                    notifyProgressDetailed (decimal td.progressDetailed) |> ignore
                    c td |> ignore
                    true
                else false
            | None -> false

        /// kk:20200410 - Note that we have to resort to using exceptions for flow control here.
        /// There seems to be no other easy and clean way. Revisit if that changes.
        /// Follow the trail of that date stamp to find other related places.
        let checkCancellation() =
            let fromLastCheck = DateTime.Now - lastCheck
            //printfn "checkCancellation: runQueueId = %A, time interval from last check = %A." n.runQueueId fromLastCheck

            if fromLastCheck > n.checkFreq
            then
                lastCheck <- DateTime.Now
                let cancel = n.checkCancellation n.runQueueId

                match cancel with
                | Some c -> raise(ComputationAbortedException (n.runQueueId, c))
                | None -> ()

        let needsCallBack (t : double) : bool =
            true

        let callBack (t : double) (x : double[]) =
            callCount <- callCount + 1L
            calculated <- false
            checkCancellation()

            match notifyTime t x (shouldNotify()) with
            | true -> ()
            | false ->
                match p.noOfProgressPoints with
                | Some k when k > 0 && n.tEnd > 0.0 ->
                    if t > (double progressCount) * (n.tEnd / (double k))
                    then
                        progressCount <- ((double k) * (t / n.tEnd) |> int) + 1
                        //printfn "Step: %A, time: %A,%s t: %A of %A, modelDataId: %A." progressCount (DateTime.Now) (estCompl start progressCount k) t n.tEnd n.modelDataId
                        notifyProgress t progressCount k |> ignore
                        notifyTime t x true |> ignore
                | _ -> ()

            // Tries to capture some chart data at the start of the run.
            // This is used when we have a super slow model and want to know what's going on.
            let tryNotifyChartEarly() =
                if outputCount <= 5 && shouldNotify() then notifyChart t x
                else ()

            match p.noOfOutputPoints with
            | Some k when k > 0 ->
                if t > (double outputCount) * (n.tEnd / (double k))
                then
                    outputCount <- ((double k) * (t / n.tEnd) |> int) + 1
                    notifyChart t x
                else tryNotifyChartEarly()
            | _ -> ()

        let cashCarpDerivative (x : double[]) (t : double) : double[] =
            callBack t x
            n.calculationData.getDerivative x

        let functionalInterop() = createInterop(callBack, n.calculationData.modelIndices)
        let chordWithDiagonalJacobianInterop() = createInterop1(needsCallBack, callBack, n.calculationData.modelIndices)

        notifyProgress 0.0 progressCount (p.noOfProgressPoints |> Option.defaultValue defaultNoOfProgressPoints) |> ignore

        match n.solverType with
        | AlgLib CashCarp ->
            printfn "nSolve: Using Cash - Carp Alglib solver."

            let nt = 2
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
                // TODO kk:20210316 - UseNonNegative is hardcoded below.
                OdeSolver.RunFSharp((fun() -> functionalInterop()), m.value, i.value, p.startTime, p.endTime, n.initialValues, (fun r e -> mapResults r e))
            | ChordWithDiagonalJacobian ->
                // TODO kk:20210316 - DoNotCorrect is hardcoded below.
                OdeSolver.RunFSharp((fun() -> chordWithDiagonalJacobianInterop()), m.value, i.value, p.startTime, p.endTime, n.initialValues, (fun r e -> mapResults r e))
