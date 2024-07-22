namespace ClmSys

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open GeneralPrimitives
open Primitives.GeneralPrimitives
open WorkerNodePrimitives
open Softellect.Messaging.Primitives
open Softellect.Sys.DataAccess
open Softellect.Sys.Errors
open ClmSys.ContGenPrimitives
open Softellect.Messaging.Errors

/// Collection of general errors & related functionality.
module GeneralErrors =

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


    type TryResetRunQueueError =
        | TryResetRunQueueDbErr of DbError
        | ResetRunQueueEntryErr of RunQueueId


    type SaveRunQueueError =
        | SaveRunQueueDbErr of DbError


    type DeleteRunQueueError =
        | DeleteRunQueueEntryErr of RunQueueId
        | DeleteRunQueueDbErr of DbError


    type TryUpdateRunQueueRowError =
        | InvalidStatusTransitionErr of InvalidRunQueueData
        | InvalidDataErr of InvalidRunQueueData
        | TryUpdateRunQueueRowDbErr of DbError


    type UpsertRunQueueError =
        | UpsertRunQueueDbErr of DbError


    type LoadWorkerNodeInfoError =
        | LoadWorkerNodeInfoDbErr of DbError
        | UnableToLoadWorkerNodeErr of WorkerNodeId


    type UpsertWorkerNodeInfoError =
        | UpsertWorkerNodeInfoDbErr of DbError


    type UpsertWorkerNodeErrError =
        | UpsertWorkerNodeErrDbErr of DbError


    type TryGetAvailableWorkerNodeError =
        | TryGetAvailableWorkerNodeDbErr of DbError


    type LoadClmDefaultValueError =
        | LoadClmDefaultValueDbErr of DbError
        | CannotLoadClmDefaultValue of ClmDefaultValueId


    type UpsertClmDefaultValueError =
        | UpsertClmDefaultValueDbErr of DbError
        | CannotUpsertClmDefaultValue of ClmDefaultValueId


    type LoadCommandLineParamsError =
        | LoadCommandLineParamsDbErr of DbError


    type AddCommandLineParamsError =
        | AddCommandLineParamsDbErr of DbError


    type TryCreateClmTaskError =
        | TryCreateClmTaskDbErr of DbError
        | CannotCreateClmTask of ClmTaskId
        | ErrorWhenCreatingClmTask of ClmTaskId


    type LoadClmTaskError =
        | LoadClmTaskDbErr of DbError
        | CannotLoadClmTask of ClmTaskId


    type LoadIncompleteClmTasksError =
        | LoadIncompleteClmTasksDbErr of DbError


    type AddClmTaskError =
        | AddClmTaskDbErr of DbError


    type UpdateClmTaskError =
        | UpdateClmTaskDbErr of DbError
        | CannotUpdateClmTask of ClmTaskId


    type TryCreateModelDataError =
        | TryCreateModelDataDbErr of DbError
        | ErrorWhenCreatingModelData of ModelDataId


    type LoadModelDataError =
        | LoadModelDataDbErr of DbError
        | CannotLoadModelData of ModelDataId


    type UpsertModelDataError =
        | UpsertModelDataDbErr of DbError
        | CannotUpsertModelData of ModelDataId


    type MapRunQueueError =
        | MapRunQueueDbErr of DbError
        | CannotMapRunQueue of RunQueueId


    type LoadRunQueueError =
        | LoadRunQueueDbErr of DbError


    type LoadRunQueueProgressError =
        | LoadRunQueueProgressDbErr of DbError


    type TryLoadFirstRunQueueError =
        | TryLoadFirstRunQueueDbErr of DbError


    type TryLoadRunQueueError =
        | TryLoadRunQueueDbErr of DbError
        | InvalidRunQueueStatus of RunQueueId * int
        | ExnWhenTryLoadRunQueue of RunQueueId * exn
        | UnableToFindRunQueue of RunQueueId


    // ==========
    // WorkerNode

    type TryLoadSolverRunnersError =
        | TryLoadSolverRunnersDbErr of DbError


    type TryGetRunningSolversCountError =
        | TryGetRunningSolversCountDbErr of DbError


    type TryPickRunQueueError =
        | TryPickRunQueueDbErr of DbError


    type LoadAllActiveRunQueueIdError =
        | LoadAllActiveRunQueueIdDbErr of DbError


    type TryStartRunQueueError =
        | TryStartRunQueueDbErr of DbError
        | CannotStartRunQueue of RunQueueId


    type TryCompleteRunQueueError =
        | TryCompleteRunQueueDbErr of DbError
        | CannotCompleteRunQueue of RunQueueId


    type TryCancelRunQueueError =
        | TryCancelRunQueueDbErr of DbError
        | CannotCancelRunQueue of RunQueueId


    type TryFailRunQueueError =
        | TryFailRunQueueDbErr of DbError
        | CannotFailRunQueue of RunQueueId


    type TryRequestCancelRunQueueError =
        | TryRequestCancelRunQueueDbErr of DbError
        | CannotRequestCancelRunQueue of RunQueueId


    type TryNotifyRunQueueError =
        | TryNotifyRunQueueDbErr of DbError
        | CannotNotifyRunQueue of RunQueueId


    type TryCheckCancellationError =
        | TryCheckCancellationDbErr of DbError


    type TryCheckNotificationError =
        | TryCheckNotificationDbErr of DbError
        | CannotCheckNotification of RunQueueId


    type TryClearNotificationError =
        | TryClearNotificationDbErr of DbError
        | CannotClearNotification of RunQueueId


    type TryUpdateProgressError =
        | TryUpdateProgressDbErr of DbError
        | CannotUpdateProgress of RunQueueId


    type SendMessageError =
        | MessagingErr of MessagingError


    //type ClmDbError =
    //    | Abc

        //| DbExn of exn
        //| LoadModelDataError of Guid
        //| SaveResultDataErr of Guid
        //| LoadResultDataErr of Guid
        //| 
        //| UpsertClmDefaultValueErr of Int64
        //| LoadClmTaskByDefaultErr of Int64
        //| LoadClmTaskErr of Guid
        //| UpdateClmTaskErr of Guid
        //| UpdateModelDataErr of Guid
        //| ClmTaskTryCreatErr of Guid
        //| ModelDataTryCreateErr of Guid
        //| 
        //| 
        //| MapRunQueueErr of Guid
        //| 
        //| RunQueueTryUpdateRowErr of RunQueueTryUpdateRowError
        //| TryStartRunQueueErr of RunQueueId
        //| TryCompleteRunQueueErr of RunQueueId
        //| TryCancelRunQueueErr of RunQueueId
        //| TryFailRunQueueErr of RunQueueId
        //| TryPickRunQueueErr
        //| TryLoadRunQueueErr of RunQueueId * string
        //| TryRequestCancelRunQueueErr of RunQueueId
        //| TryNotifyRunQueueErr of RunQueueId
        //| TryUpdateProgressRunQueueErr of RunQueueId
        //| TryClearNotificationErr of RunQueueId
        //| TryCheckCancellationErr of RunQueueId
        //| TryCheckNotificationErr of RunQueueId


    //type RegistryErrorInfo =
    //    {
    //        version : string
    //        client : string
    //        data : string
    //    }


    //type RegistryError =
    //    | CreateRegistrySubKeyErr of string * exn
    //    | SetRegistryValueErr of string * exn
    //    | GetRegistryValueErr of string * exn
    //    | GetMessagingClientPortErr of RegistryErrorInfo
    //    | GetMessagingClientIdErr of RegistryErrorInfo
    //    | GetPartitionerMessagingClientIdErr of RegistryErrorInfo
    //    | GetUsePartitionerErr of RegistryErrorInfo
    //    | GetNumberOfCoresErr of RegistryErrorInfo
    //    | GetWrkInactiveErr of RegistryErrorInfo
    //    | GetContGenServicePortErr of RegistryErrorInfo
    //    | GetContGenMinUsefulEeErr of RegistryErrorInfo
    //    | GetWorkerNodeClientPortErr of RegistryErrorInfo


    type ClmEventHandlerError =
        | UnhandledEventHandlerExn of string * Guid * exn
        | StillRunningEventHandlerErr of string * Guid * DateTime
