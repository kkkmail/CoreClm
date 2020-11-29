namespace ClmSys

open System
open ClmSys.VersionInfo

module GeneralPrimitives =

    [<Measure>] type millisecond
    [<Measure>] type second
    [<Measure>] type minute
    [<Measure>] type hour


    let millisecondsPerSecond = 1_000<millisecond/second>
    let secondsPerMinute = 60<second/minute>
    let minutesPerHour = 60<minute/hour>


    let DefaultContGenServicePort = DefaultServicePort
    let DefaultContGenServiceAddress = "localhost"


    let DefaultWorkerNodeServicePort = 20000 + DefaultServicePort
    let DefaultWorkerNodeServiceAddress = "localhost"


    let DefaultMessagingServicePort = 40000 + DefaultServicePort
    let DefaultMessagingServiceAddress = "localhost"


    type ServiceAddress =
        | ServiceAddress of string

        member this.value = let (ServiceAddress v) = this in v


    type ServicePort =
        | ServicePort of int

        member this.value = let (ServicePort v) = this in v


    type ServiceName =
        | ServiceName of string

        member this.value = let (ServiceName v) = this in (v + " - " + versionNumberValue.value + "." + messagingDataVersion.value.ToString())
        member this.originalValue = let (ServiceName v) = this in v


    type ConnectionString =
        | ConnectionString of string

        member this.value = let (ConnectionString v) = this in v


    type SqliteConnectionString =
        | SqliteConnectionString of string

        member this.value = let (SqliteConnectionString v) = this in v


    type ResultDataId =
        | ResultDataId of Guid

        member this.value = let (ResultDataId v) = this in v


    type RunQueueId =
        | RunQueueId of Guid

        member this.value = let (RunQueueId v) = this in v
        member this.toResultDataId() = this.value |> ResultDataId
        static member getNewId() = Guid.NewGuid() |> RunQueueId


    type ErrorMessage =
        | ErrorMessage of string

        member this.value = let (ErrorMessage v) = this in v


    type RunQueueStatus =
        | NotStartedRunQueue
        | InactiveRunQueue
        | RunRequestedRunQueue
        | InProgressRunQueue
        | CompletedRunQueue
        | FailedRunQueue
        | CancelRequestedRunQueue
        | CancelledRunQueue
        | InvalidRunQueue // It does not exist in DB, so it it not possible to insert it due to FK constraint.

        member r.value =
            match r with
            | NotStartedRunQueue -> 0
            | InactiveRunQueue -> 1
            | RunRequestedRunQueue -> 7
            | InProgressRunQueue -> 2
            | CompletedRunQueue -> 3
            | FailedRunQueue -> 4
            | CancelRequestedRunQueue -> 5
            | CancelledRunQueue -> 6
            | InvalidRunQueue -> -1000

        static member tryCreate i =
            match i with
            | 0 -> Some NotStartedRunQueue
            | 1 -> Some InactiveRunQueue
            | 7 -> Some RunRequestedRunQueue
            | 2 -> Some InProgressRunQueue
            | 3 -> Some CompletedRunQueue
            | 4 -> Some FailedRunQueue
            | 5 -> Some CancelRequestedRunQueue
            | 6 -> Some CancelledRunQueue
            | _ -> None


    type SerializationFormat =
        | BinaryFormat
        | BinaryZippedFormat
        | JSonFormat
        | XmlFormat

        member format.fileExtension =
            match format with
            | BinaryFormat -> "bin"
            | BinaryZippedFormat -> "binz"
            | JSonFormat -> "json"
            | XmlFormat -> "xml"
