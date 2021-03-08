namespace OdeSolver

open System.Threading
open Clm.ChartData
open Microsoft.FSharp.Core
open System
open ClmSys.GeneralPrimitives
open ClmSys.GeneralData
open ClmSys.SolverRunnerPrimitives
open ClmSys.ClmErrors
open ClmSys.SolverData


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


    type NSolveParam =
        {
            modelDataId : Guid
            runQueueId : RunQueueId
            tStart : double
            tEnd : double
            derivative : double[] -> double[]
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

        let notifyProgress t r m =
//            Thread.Sleep(30_000)
            match n.progressCallBack with
            | Some c -> calculateProgress r m |> c
            | None -> Ok()

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

                    c td |> ignore
                else ()
            | None -> ()

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

        let shouldNotify() =
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

        let f (x : double[]) (t : double) : double[] =
            callCount <- callCount + 1L
            calculated <- false
            checkCancellation()
            notifyTime t x false

            match p.noOfProgressPoints with
            | Some k when k > 0 && n.tEnd > 0.0 ->
                if t > (double progressCount) * (n.tEnd / (double k))
                then
                    progressCount <- ((double k) * (t / n.tEnd) |> int) + 1
                    //printfn "Step: %A, time: %A,%s t: %A of %A, modelDataId: %A." progressCount (DateTime.Now) (estCompl start progressCount k) t n.tEnd n.modelDataId
                    notifyProgress t progressCount k |> ignore
                    notifyTime t x true
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

            n.derivative x

        let nt = 2
        let x : array<double> = [| for i in 0..nt -> p.startTime + (p.endTime - p.startTime) * (double i) / (double nt) |]
        let d = alglib.ndimensional_ode_rp (fun x t y _ -> f x t |> Array.mapi(fun i e -> y.[i] <- e) |> ignore)
        let mutable s = alglib.odesolverrkck(n.initialValues, x, p.eps, p.stepSize)
        do alglib.odesolversolve(s, d, null)
        let mutable (m, xTbl, yTbl, rep) = alglib.odesolverresults(s)

        {
            startTime = p.startTime
            endTime = p.endTime
            xEnd = yTbl.[nt - 1, *]
        }


//    type PartitionType =
//        | InsideInterval
//        | EndOfInterval
//        | OutsideInterval
//
//
//    type PartitionInfo =
//        {
//            partitionType : PartitionType
//            getNSolveParam : double[] -> NSolveParam
//        }
//
//
//    let defaultPartition (n : NSolveParam) : List<PartitionInfo> =
//        let p =
//            [
//                (30.0, InsideInterval)
//                (75.0, InsideInterval)
//                (150.0, InsideInterval)
//                (250.0, EndOfInterval)
//                (400.0, OutsideInterval)
//                (600.0, OutsideInterval)
//                (1000.0, OutsideInterval)
//            ]
//
//        let s =
//            match p |> List.tryPick (fun (a, b) -> match b with | EndOfInterval -> Some a | _ -> None) with
//            | Some a -> a
//            | None -> 250.0
//
//        p
//        |> List.mapi (fun i (a, b) ->
//                    {
//                        partitionType = b
//                        getNSolveParam =
//                            if i = 0
//                            then fun x -> { n with initialValues = x; tStart = n.tStart; tEnd = n.tEnd * a / s }
//                            else n.next (n.tEnd * a / s)
//                    })
//
//
//    type ContinueRunParam =
//        {
//            maxWeightedAverageAbsEeMaxThreshold : double
//            maxLastEeMaxThreshold : double
//            maxWeightedAverageAbsEeMniThreshold : double
//            maxLastEeMinThreshold : double
//        }
//
//        static member defaultValue =
//            {
//                maxWeightedAverageAbsEeMaxThreshold = 0.500
//                maxLastEeMaxThreshold = 0.500
//                maxWeightedAverageAbsEeMniThreshold = 0.000_100
//                maxLastEeMinThreshold = 0.000_100
//            }
//
//
//    /// The following rules are used:
//    ///     1. FOR (any interval):
//    ///        IF (maxWeightedAverageAbsEe >= maxWeightedAverageAbsEeMaxThreshold OR maxLastEe >= maxLastEeMaxThreshold)
//    ///        THEN STOP.
//    ///
//    ///     2. IF EndOfInterval
//    ///        AND (maxWeightedAverageAbsEe < maxWeightedAverageAbsEeMniThreshold AND maxLastEe < maxLastEeMinThreshold)
//    ///        THEN STOP.
//    let continueRun c n p =
//        match n.getEeData |> Option.bind(fun x -> x() |> Some) with
//        | Some d ->
//            let isEndOfInterval() =
//                match p.partitionType with
//                | EndOfInterval -> true
//                | _ -> false
//
//            let rules (e : EeData) =
//                [
//                    e.maxWeightedAverageAbsEe >= c.maxWeightedAverageAbsEeMaxThreshold || e.maxLastEe >= c.maxLastEeMaxThreshold
//                    isEndOfInterval() && (e.maxWeightedAverageAbsEe < c.maxWeightedAverageAbsEeMniThreshold || e.maxLastEe < c.maxLastEeMinThreshold)
//                ]
//
//            let stop = rules d |> List.fold (fun acc r -> r || acc) false
//            not stop
//        | None ->
//            match p.partitionType with
//            | InsideInterval -> true
//            | EndOfInterval -> false
//            | OutsideInterval -> false
//
//
//    let defaultContinueRun = continueRun ContinueRunParam.defaultValue
//
//
//    type OdeController =
//        {
//            partition : NSolveParam -> List<PartitionInfo>
//            continueRun : NSolveParam -> PartitionInfo -> bool // This function is NOT pure because there is a MailboxProcessor behind it.
//        }
//
//        static member defaultValue =
//            {
//                partition = defaultPartition
//                continueRun = defaultContinueRun
//            }
//
//
//    type NSolvePartitionParam =
//        {
//            nSolveParam : NSolveParam
//            controller : OdeController
//        }
//
//        static member defaultValue n =
//            {
//                nSolveParam = n
//                controller = OdeController.defaultValue
//            }
//
//
//    let nSolvePartitioned (p : NSolvePartitionParam) : unit =
//        let d = p.controller.partition p.nSolveParam
//
//        let x =
//            d
//            |> List.fold(fun (a, b) e ->
//                            if b
//                            then
//                                let r = e.getNSolveParam a |> nSolve
//                                (r.xEnd, p.controller.continueRun p.nSolveParam e)
//                            else (a, b)) (p.nSolveParam.initialValues, true)
//
//        ()
