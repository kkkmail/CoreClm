namespace GenericOdeSolver

open System
open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives

module Primitives =

    type CallBack =
        | CallBack of (double -> double[] -> unit)

        member r.invoke = let (CallBack v) = r in v


    type NeedsCallBackChecker =
        | NeedsCallBackChecker of (double -> CancellationType option * bool)

        member r.invoke = let (NeedsCallBackChecker v) = r in v


    type CancellationChecker =
        | CancellationChecker of (RunQueueId -> CancellationType option)

        member r.invoke = let (CancellationChecker v) = r in v


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


    type OdeParams =
        {
            startTime : double
            endTime : double
            stepSize : double
            absoluteTolerance : AbsoluteTolerance
            solverType : SolverType

            // noOfOutputPoints : int
            // noOfProgressPoints : int
            // noOfChartDetailedPoints : int option
        }


    type OdeResult<'T> =
        {
            startTime : double
            endTime : double
            xEnd : double[]
            data : 'T
        }


    type NSolveParam<'T> =
        {
            odeParams : OdeParams
            runQueueId : RunQueueId
            initialValues : double[]
            derivative : DerivativeCalculator
            callBack : CallBack
            checkFreq : TimeSpan
            checkCancellation : CancellationChecker
            needsCallBack : NeedsCallBackChecker
            getData : double -> double[] -> 'T
            started : DateTime
        }
