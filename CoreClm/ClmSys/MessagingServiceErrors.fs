namespace ClmSys

open System
open VersionInfo
open GeneralErrors
open MessagingPrimitives
open MessagingCommonErrors

module MessagingServiceErrors =

    type MessageCreateError =
        | InvalidDeliveryTypeErr of int
        | InvalidDataVersionErr of VersionMismatchInfo
        | InvalidDeliveryTypeAndDataVersionErr of int * VersionMismatchInfo


    type MessageUpsertError =
        | CannotUpsertMessageErr of MessageId

    type MessageDeleteError =
        | CannotDeleteMessageErr of MessageId

    type MsgSvcDbError =
        | MessageCreateErr of MessageCreateError
        | MessageDeleteErr of MessageDeleteError


    type GetVersionSvcError =
        | GetVersionSvcWcfErr of WcfError


    type ConfigureServiceError =
        | CfgSvcWcfErr of WcfError


    type TryPeekMessageError =
        | TryPeekMsgWcfErr of WcfError
        | UnableToLoadMessageErr of (MessagingClientId * MessageId)


    type MessageDeliveryError =
        | ServiceNotStartedErr
        | ServerIsShuttingDownErr
        | DataVersionMismatchErr of MessagingDataVersion
        | MsgWcfErr of WcfError


    type TryDeleteFromServerError =
        | TryDeleteMsgWcfErr of WcfError
        | CannotFindClientErr of Guid
        | UnableToDeleteMessageErr of (MessagingClientId * MessageId)
        
        
    type MsgSettingsError =
        | InvalidSettings of string
        | MsgSettingExn of exn


    type MessagingServiceError =
        | MsgSvcDbErr of MsgSvcDbError
        | GetVersionSvcErr of GetVersionSvcError
        | MessageDeliveryErr of MessageDeliveryError
        | MessageUpsertErr of MessageUpsertError
        | TryPeekMessageErr of TryPeekMessageError
        | TryDeleteFromServerErr of TryDeleteFromServerError
        | MsgSettingsErr of MsgSettingsError
