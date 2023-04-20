namespace GenericOdeSolver

open System
open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives

module Primitives =

    type CalculationCompletionType =
        | CompletedCalculation
        | CancelledCalculation of CancellationType


    type CallBackNotificationType =
        | ProgressNotification
        | ChartNotification
        | ProgressAndChartNotification


    type CallBackType =
        | RegularCallBack
        | FinalCallBack of CalculationCompletionType


    type CallBackData =
        {
            progressData : ProgressData
            t : double
            x : double[]
        }


    /// A function to call in order to notify about progress.
    type ProgressCallBack =
        | ProgressCallBack of (CallBackType -> CallBackData -> unit)

        member r.invoke = let (ProgressCallBack v) = r in v


    /// A function to call in order to generate a chart data point.
    type ChartCallBack =
        | ChartCallBack of (CallBackType -> CallBackData -> unit)

        member r.invoke = let (ChartCallBack v) = r in v


    /// A function to call to check if cancellation is requested.
    type CheckCancellation =
        | CheckCancellation of (RunQueueId -> CancellationType option)

        member r.invoke = let (CheckCancellation v) = r in v


    /// An addition [past] data needed to determine if a call back is needed.
    type NeedsCallBackData =
        {
            progressData : ProgressData
            lastCheck : DateTime
            nextProgress : decimal
            nextChartProgress : decimal
        }

        static member defaultValue =
            {
                progressData =
                    {
                        callCount = 0L
                        progress = 0.0M
                        errorMessageOpt = None
                    }
                lastCheck = DateTime.Now
                nextProgress = 0.0M
                nextChartProgress = 0.0M
            }


    /// A function to call in order to determine if a call back is needed.
    type NeedsCallBack =
        | NeedsCallBack of (NeedsCallBackData -> double -> NeedsCallBackData * CallBackNotificationType option)

        member r.invoke = let (NeedsCallBack v) = r in v


    type CallBackInfo =
        {
            checkFreq : TimeSpan
            needsCallBack : NeedsCallBack
            progressCallBack : ProgressCallBack
            chartCallBack : ChartCallBack
            checkCancellation : CheckCancellation
        }


    type DerivativeCalculator =
        | OneByOne of (double -> double[] -> int -> double)
        | FullArray of (double -> double[] -> double[])

        member d.calculate t x =
            match d with
            | OneByOne f -> x |> Array.mapi (fun i _ -> f t x i)
            | FullArray f -> f t x

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


    type OdeOutputParams =
        {
            noOfOutputPoints : int
            noOfProgressPoints : int
            noOfChartDetailedPoints : int option
        }


    type OdeParams =
        {
            startTime : double
            endTime : double
            stepSize : double
            absoluteTolerance : AbsoluteTolerance
            solverType : SolverType
            outputParams : OdeOutputParams
        }


    // type OdeResult =
    //     {
    //         progressDataEnd : ProgressData
    //         tEnd : double
    //         xEnd : double[]
    //     }


    type NSolveParam =
        {
            odeParams : OdeParams
            runQueueId : RunQueueId
            initialValues : double[]
            derivative : DerivativeCalculator
            callBackInfo : CallBackInfo
            started : DateTime
        }
