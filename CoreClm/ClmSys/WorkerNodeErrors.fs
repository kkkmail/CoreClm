namespace ClmSys

open Primitives.GeneralPrimitives
open Softellect.Wcf.Errors
open Softellect.Messaging.Primitives
open Softellect.Messaging.Errors

open GeneralPrimitives

module WorkerNodeErrors =
    let x = 1

    //type OnRunModelError =
    //    | CannotRunModelErr of RunQueueId
    //    | CannotDeleteRunQueueErr of RunQueueId


//    type OnProcessMessageError =
//        | CannotSaveModelDataErr of MessageId * RunQueueId
////        | OnRunModelFailedErr of MessageId * RunQueueId
////        | ModelAlreadyRunningErr of MessageId * RunQueueId
//        | InvalidMessageErr of (MessageId * string)
//        | FailedToCancelErr of (MessageId * RunQueueId * exn)


//    type OnRequestResultError =
//        | CannotFindRunQueueErr of RunQueueId


//    //type WrkSettingsError =
//    //    | InvalidSettings of string
//    //    | WrkSettingExn of exn


//    type WorkerNodeError =
//        | OnRunModelErr of OnRunModelError
//        | OnProcessMessageErr of OnProcessMessageError
//        | OnGetMessagesErr of OnGetMessagesError
//        | OnRequestResultErr of OnRequestResultError
//        //| WrkSettingsErr of WrkSettingsError


//    type WorkerNodeWcfError =
//        | ConfigureWcfErr of WcfError
//        | MonitorWcfErr of WcfError
//        | PingWcfErr of WcfError


//    type WorkerNodeServiceError =
//        | WorkerNodeWcfErr of WorkerNodeWcfError
//        | UnableToStartMessagingClientErr of MessagingError
//        | UnableToCreateWorkerNodeServiceErr
//        | ServiceUnavailableErr
//        | UpdateLocalProgressErr of string
//        | ConfigureServiceErr of string
//        | MonitorServiceErr of string

//        // ======
//        // Ugly stuff
//        // TODO kk:20240722 - WorkNode error types are now inconsistent and conflict with messaging error types.
//        // See: https://github.com/kkkmail/CoreClm/issues/40
//        | UnableToREgisterWorkerNodeErr of MessagingError
//        | CreateServiceImplWorkerNodeErr of MessagingError
