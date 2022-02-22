namespace ClmSys

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open GeneralPrimitives
open WorkerNodePrimitives
open ContGenPrimitives
open Softellect.Sys.MessagingPrimitives

/// Collection of general errors & related functionality.
module GeneralErrors =

    type ErrorId =
        | ErrorId of Guid

        static member getNewId() = Guid.NewGuid() |> ErrorId
        member this.value = let (ErrorId v) = this in v


    type TraceInfo =
        {
            memberName : string
            sourcePath : string
            sourceLine : int
        }


    type Tracer() =
        member _.doTrace([<CallerMemberName; Optional; DefaultParameterValue("")>] memberName: string,
                          [<CallerFilePath; Optional; DefaultParameterValue("")>] path: string,
                          [<CallerLineNumber; Optional; DefaultParameterValue(0)>] line: int) =

            {
                memberName = memberName
                sourcePath = path
                sourceLine = line
            }


    let tracer = Tracer()


    type FileError =
        | GeneralFileExn of exn
        | GetFolderNameExn of exn
        | GetFileNameExn of exn
        | FileNotFoundErr of string
        | ReadFileExn of exn
        | WriteFileExn of exn
        | DeleteFileExn of exn
        | GetObjectIdsExn of exn
        | CreateChartsExn of exn
        | SaveChartsExn of exn


    type JsonParseError =
        | InvalidStructureErr of string


    type SerializationError =
        | SerializationExn of exn
        | DeserializationExn of exn


    type InvalidRunQueueData =
        {
            runQueueId : RunQueueId
            runQueueStatusFrom : RunQueueStatus option
            runQueueStatusTo : RunQueueStatus
            workerNodeIdOptFrom : WorkerNodeId option
            workerNodeIdOptTo : WorkerNodeId option
        }


    type RunQueueTryUpdateRowError =
        | InvalidStatusTransitionErr of InvalidRunQueueData
        | InvalidDataErr of InvalidRunQueueData


    type DbError =
        | DbExn of exn
        | LoadModelDataError of Guid
        | SaveResultDataErr of Guid
        | LoadResultDataErr of Guid
        | LoadClmDefaultValueErr of Int64
        | UpsertClmDefaultValueErr of Int64
        | LoadClmTaskByDefaultErr of Int64
        | LoadClmTaskErr of Guid
        | UpdateClmTaskErr of Guid
        | UpdateModelDataErr of Guid
        | ClmTaskTryCreatErr of Guid
        | ModelDataTryCreateErr of Guid
        | DeleteRunQueueEntryErr of RunQueueId
        | ResetRunQueueEntryErr of RunQueueId
        | MapRunQueueErr of Guid
        | LoadWorkerNodeInfoErr of Guid
        | RunQueueTryUpdateRowErr of RunQueueTryUpdateRowError
        | TryStartRunQueueErr of RunQueueId
        | TryCompleteRunQueueErr of RunQueueId
        | TryCancelRunQueueErr of RunQueueId
        | TryFailRunQueueErr of RunQueueId
        | TryPickRunQueueErr
        | TryLoadRunQueueErr of RunQueueId * string
        | TryRequestCancelRunQueueErr of RunQueueId
        | TryNotifyRunQueueErr of RunQueueId
        | TryUpdateProgressRunQueueErr of RunQueueId
        | TryClearNotificationErr of RunQueueId
        | TryCheckCancellationErr of RunQueueId
        | TryCheckNotificationErr of RunQueueId
        | MessagingSvcSaveMessageErr of MessageId
        | MessagingSvcCannotDeleteMessageErr of MessageId

    type ServiceInstallerError =
        | InstallServiceErr of exn
        | UninstallServiceErr of exn
        | StartServiceErr of exn
        | StopServiceErr of exn


    type RegistryErrorInfo =
        {
            version : string
            client : string
            data : string
        }


    type RegistryError =
        | CreateRegistrySubKeyErr of string * exn
        | SetRegistryValueErr of string * exn
        | GetRegistryValueErr of string * exn
        | GetMessagingClientPortErr of RegistryErrorInfo
        | GetMessagingClientIdErr of RegistryErrorInfo
        | GetPartitionerMessagingClientIdErr of RegistryErrorInfo
        | GetUsePartitionerErr of RegistryErrorInfo
        | GetNumberOfCoresErr of RegistryErrorInfo
        | GetWrkInactiveErr of RegistryErrorInfo
        | GetContGenServicePortErr of RegistryErrorInfo
        | GetContGenMinUsefulEeErr of RegistryErrorInfo
        | GetWorkerNodeClientPortErr of RegistryErrorInfo


    type ClmEventHandlerError =
        | UnhandledEventHandlerExn of string * Guid * exn
        | StillRunningEventHandlerErr of string * Guid * DateTime
