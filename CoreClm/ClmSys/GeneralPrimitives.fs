namespace ClmSys

open System
open Primitives.VersionInfo

module GeneralPrimitives =

    [<Measure>] type millisecond
    [<Measure>] type second
    [<Measure>] type minute
    [<Measure>] type hour


    let millisecondsPerSecond = 1_000<millisecond/second>
    let secondsPerMinute = 60<second/minute>
    let minutesPerHour = 60<minute/hour>

    let localHost = "127.0.0.1"


    let defaultContGenNetTcpServicePort = defaultServicePort
    let defaultContGenHttpServicePort = defaultContGenNetTcpServicePort + 1
    let defaultContGenServiceAddress = localHost


    let defaultWorkerNodeNetTcpServicePort = 20000 + defaultServicePort
    let defaultWorkerNodeHttpServicePort = defaultWorkerNodeNetTcpServicePort + 1
    let defaultWorkerNodeServiceAddress = localHost


    let defaultMessagingNetTcpServicePort = 40000 + defaultServicePort
    let defaultMessagingHttpServicePort = defaultMessagingNetTcpServicePort + 1
    let defaultMessagingServiceAddress = localHost


    type ConnectionString =
        | ConnectionString of string

        member this.value = let (ConnectionString v) = this in v


    type SqliteConnectionString =
        | SqliteConnectionString of string

        member this.value = let (SqliteConnectionString v) = this in v


    // type RunQueueId =
    //     | RunQueueId of Guid
    //
    //     member this.value = let (RunQueueId v) = this in v
    //     static member getNewId() = Guid.NewGuid() |> RunQueueId
    //
    //
    // type ErrorMessage =
    //     | ErrorMessage of string
    //
    //     member this.value = let (ErrorMessage v) = this in v
    //
    //
    // type RunQueueStatus =
    //     | NotStartedRunQueue
    //     | InactiveRunQueue
    //     | RunRequestedRunQueue
    //     | InProgressRunQueue
    //     | CompletedRunQueue
    //     | FailedRunQueue
    //     | CancelRequestedRunQueue
    //     | CancelledRunQueue
    //
    //     member r.value =
    //         match r with
    //         | NotStartedRunQueue -> 0
    //         | InactiveRunQueue -> 1
    //         | RunRequestedRunQueue -> 7
    //         | InProgressRunQueue -> 2
    //         | CompletedRunQueue -> 3
    //         | FailedRunQueue -> 4
    //         | CancelRequestedRunQueue -> 5
    //         | CancelledRunQueue -> 6
    //
    //     static member tryCreate i =
    //         match i with
    //         | 0 -> Some NotStartedRunQueue
    //         | 1 -> Some InactiveRunQueue
    //         | 7 -> Some RunRequestedRunQueue
    //         | 2 -> Some InProgressRunQueue
    //         | 3 -> Some CompletedRunQueue
    //         | 4 -> Some FailedRunQueue
    //         | 5 -> Some CancelRequestedRunQueue
    //         | 6 -> Some CancelledRunQueue
    //         | _ -> None
