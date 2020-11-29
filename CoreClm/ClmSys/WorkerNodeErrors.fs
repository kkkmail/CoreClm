namespace ClmSys

open GeneralPrimitives
open MessagingClientErrors
open MessagingPrimitives
open GeneralErrors

module WorkerNodeErrors =

    type OnSaveResultError =
        | SendResultMessageError of (MessagingClientId * ResultDataId)


    type OnSaveChartsError =
        | SendChartMessageError of (MessagingClientId * ResultDataId)


    type OnUpdateProgressError =
        | UnableToSendProgressMsgErr of RunQueueId
        | UnableToFindMappingErr of RunQueueId


    type OnRunModelError =
        | CannotRunModelErr


    type OnProcessMessageError =
        | CannotSaveModelDataErr of MessageId * RunQueueId
        | OnRunModelFailedErr of MessageId * RunQueueId
        | ModelAlreadyRunningErr of MessageId * RunQueueId
        | InvalidMessageErr of (MessageId * string)
        | FailedToCancelErr of (MessageId * RunQueueId * exn)


    type OnRequestResultError =
        | CannotFindRunQueueErr of RunQueueId
        
        
    type WrkSettingsError =
        | InvalidSettings of string
        | WrkSettingExn of exn
        

    type WorkerNodeError =
        | OnSaveResultErr of OnSaveResultError
        | OnSaveChartsErr of OnSaveChartsError
        | OnUpdateProgressErr of OnUpdateProgressError
        | OnRunModelErr of OnRunModelError
        | OnProcessMessageErr of OnProcessMessageError
        | OnGetMessagesErr of OnGetMessagesError
        | OnRequestResultErr of OnRequestResultError
        | WrkSettingsErr of WrkSettingsError


    type WorkerNodeWcfError =
        | ConfigureWcfErr of WcfError
        | MonitorWcfErr of WcfError
        | PingWcfErr of WcfError


    type WorkerNodeServiceError =
        | WorkerNodeWcfErr of WorkerNodeWcfError
        | UnableToStartMessagingClientErr
        | UnableToCreateWorkerNodeServiceErr
        | ServiceUnavailableErr
        | UpdateLocalProgressErr of string
        | ConfigureServiceErr of string
        | MonitorServiceErr of string
