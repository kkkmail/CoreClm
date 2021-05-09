﻿namespace ClmSys

open System

open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.Primitives

open GeneralPrimitives
open WorkerNodePrimitives

module ContGenPrimitives =

    [<Literal>]
    let DefaultMinEe = 0.000_1


    [<Literal>]
    let DefaultAbsoluteTolerance = 1.0e-08


    type ContGenServiceName =
        | ContGenServiceName of ServiceName

        member this.value = let (ContGenServiceName v) = this in v
        static member httpServiceName = "ContGenHttpService" |> ServiceName |> ContGenServiceName
        static member netTcpServiceName = "ContGenNetTcpService" |> ServiceName |> ContGenServiceName


    type MinUsefulEe =
        | MinUsefulEe of double

        member this.value = let (MinUsefulEe v) = this in v
        static member defaultValue = MinUsefulEe DefaultMinEe


    type AbsoluteTolerance =
        | AbsoluteTolerance of double

        member this.value = let (AbsoluteTolerance v) = this in v
        static member defaultValue = AbsoluteTolerance DefaultAbsoluteTolerance


    type ModelDataId =
        | ModelDataId of Guid

        member this.value = let (ModelDataId v) = this in v
        static member getNewId() = Guid.NewGuid() |> ModelDataId


    type ClmDefaultValueId =
        | ClmDefaultValueId of int64

        member df.value = let (ClmDefaultValueId v) = df in v
        override df.ToString() = df.value.ToString().PadLeft(9, '0') + "L"


    type ClmTaskStatus =
        | ActiveClmTask
        | InactiveClmTask

        member s.value =
            match s with
            | ActiveClmTask -> 0
            | InactiveClmTask -> 1

        static member tryCreate i =
            match i with
            | 0 -> Some ActiveClmTask
            | 1 -> Some InactiveClmTask
            | _ -> None


    type ClmTaskId =
        | ClmTaskId of Guid

        member this.value = let (ClmTaskId v) = this in v
        static member getNewId() = Guid.NewGuid() |> ClmTaskId


    type HtmlChart =
        {
            htmlContent: string
            fileName : string
        }


    type ChartInfo =
        {
            runQueueId : RunQueueId
            defaultValueId : ClmDefaultValueId
            charts : list<HtmlChart>
        }


    type ChartGenerationResult =
        | GeneratedCharts of ChartInfo
        | NotGeneratedCharts


//    type TaskProgress =
////        | NotStarted
////        | InProgress of double
////        | Completed of (double option * string option)
////        | Failed of ErrorMessage
////        | Cancelled of string option
////        | AllCoresBusy of WorkerNodeId
//
//        | NotStarted
//        | InProgress
//        | Completed
//        | Failed
//        | Cancelled
//        | AllCoresBusy


//        static member notStartedValue = 0.0
//        static member failedValue = -1000.0
//
//        static member create p =
//            match p with
//            | d when d <= 0.0 -> NotStarted
//            | d when d < 1.0 -> InProgress p
//            | _ -> InProgress 1.0
//
//        member progress.value =
//            match progress with
//            | NotStarted -> 0.0
//            | InProgress d -> max 0.0 (min d 1.0)
//            | Completed (v, _) ->
//                match v with
//                | None -> 1.0
//                | Some d -> max 0.0 (min d 1.0)
//            | Failed _ -> TaskProgress.failedValue
//            | Cancelled _ -> TaskProgress.failedValue
//            | AllCoresBusy _ -> TaskProgress.notStartedValue


    type ContGenAdmId =
        | ContGenAdmId of MessagingClientId

        member this.value = let (ContGenAdmId v) = this in v
        member this.messagingClientId = let (ContGenAdmId v) = this in v
        static member newId() = Guid.NewGuid() |> MessagingClientId |> ContGenAdmId


    /// Number of minutes for worker node errors to expire before the node can be again included in work distribution.
    type LastAllowedNodeErr =
        | LastAllowedNodeErr of int<minute>

        member this.value = let (LastAllowedNodeErr v) = this in v
        static member defaultValue = LastAllowedNodeErr 60<minute>


    /// Number of minutes between checks for early exit.
    type EarlyExitCheckFreq =
        | EarlyExitCheckFreq of int<minute>

        member this.value = let (EarlyExitCheckFreq v) = this in v
        static member defaultValue = EarlyExitCheckFreq 60<minute>
