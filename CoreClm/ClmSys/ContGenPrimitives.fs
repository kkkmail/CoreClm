namespace ClmSys

open System
open GeneralPrimitives
open MessagingPrimitives
open WorkerNodePrimitives

module ContGenPrimitives =

    [<Literal>]
    let DefaultMinEe = 0.000_1


    type ContGenServiceAddress =
        | ContGenServiceAddress of ServiceAddress

        member this.value = let (ContGenServiceAddress v) = this in v
        static member defaultValue = DefaultContGenServiceAddress |> ServiceAddress |> ContGenServiceAddress


    type ContGenServicePort =
        | ContGenServicePort of ServicePort

        member this.value = let (ContGenServicePort v) = this in v
        static member defaultValue = DefaultContGenServicePort |> ServicePort |> ContGenServicePort


    type ContGenServiceName =
        | ContGenServiceName of ServiceName

        member this.value = let (ContGenServiceName v) = this in v


    let contGenServiceName = "ContGenService" |> ServiceName |> ContGenServiceName


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
            resultDataId : ResultDataId
            defaultValueId : ClmDefaultValueId
            charts : list<HtmlChart>
        }


    type ChartGenerationResult =
        | GeneratedCharts of ChartInfo
        | NotGeneratedCharts


    type TaskProgress =
        | NotStarted
        | InProgress of decimal
        | Completed of (decimal option * string option)
        | Failed of ErrorMessage
        | Cancelled of string option
        | AllCoresBusy of WorkerNodeId

        static member notStartedValue = 0m
        static member failedValue = -1000m

        static member create d =
            match d with
            | _ when d <= 0.0m -> NotStarted
            | _ when d < 1.0m -> InProgress d
            | _ -> InProgress 1.0m

        member progress.value =
            match progress with
            | NotStarted -> 0m
            | InProgress d -> max 0m (min d 1m)
            | Completed (v, _) ->
                match v with
                | None -> 1.0m
                | Some d -> max 0m (min d 1m)
            | Failed _ -> TaskProgress.failedValue
            | Cancelled _ -> TaskProgress.failedValue
            | AllCoresBusy _ -> TaskProgress.notStartedValue


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
