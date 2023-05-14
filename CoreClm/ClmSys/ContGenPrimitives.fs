namespace ClmSys

open System

open Primitives.GeneralPrimitives
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.Primitives

open GeneralPrimitives
open WorkerNodePrimitives

module ContGenPrimitives =

    [<Literal>]
    let DefaultMinEe = 0.000_1


    type ContGenServiceName =
        | ContGenServiceName of ServiceName

        member this.value = let (ContGenServiceName v) = this in v
        static member httpServiceName = "ContGenHttpService" |> ServiceName |> ContGenServiceName
        static member netTcpServiceName = "ContGenNetTcpService" |> ServiceName |> ContGenServiceName


    type MinUsefulEe =
        | MinUsefulEe of double

        member this.value = let (MinUsefulEe v) = this in v
        static member defaultValue = MinUsefulEe DefaultMinEe


    type ModelDataId =
        | ModelDataId of Guid

        member this.value = let (ModelDataId v) = this in v
        static member getNewId() = Guid.NewGuid() |> ModelDataId


    type ClmDefaultValueId =
        | ClmDefaultValueId of int64

        member df.value = let (ClmDefaultValueId v) = df in v
        override df.ToString() = df.value.ToString().PadLeft(9, '0') + "L"


    type ClmTaskPriority =
        | ClmTaskPriority of int

        member df.value = let (ClmTaskPriority v) = df in v
        static member defaultValue = ClmTaskPriority 1_000


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


    type ChartInfo =
        {
            runQueueId : RunQueueId
            defaultValueId : ClmDefaultValueId
            charts : list<HtmlChart>
        }


    type ChartGenerationResult =
        | GeneratedCharts of ChartInfo
        | NotGeneratedCharts


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
