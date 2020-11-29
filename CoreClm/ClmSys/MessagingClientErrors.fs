namespace ClmSys

open VersionInfo
open GeneralErrors
open MessagingPrimitives
open MessagingCommonErrors

module MessagingClientErrors =

    type GetVersionError =
        | GetVersionWcfErr
        | VersionMismatchErr of VersionMismatchInfo


    type OnGetMessagesError =
        | ProcessedSuccessfullyWithInnerErr
        | ProcessedWithErr
        | ProcessedWithFailedToRemoveErr
        | FailedToProcessErr
        | BusyProcessingErr


    type SendMessageError =
        | SendMessageFailedErr
        | CannotDeleteMessageErr of MessageId


    type TryReceiveSingleMessageError =
        | TryPeekMessageErr
        | SaveMessageErr
        | TryDeleteFromServerErr


    type MessageDeliveryError =
        | ServiceNotStartedErr
        | ServerIsShuttingDownErr
        | DataVersionMismatchErr of MessagingDataVersion
        | MsgWcfErr of WcfError


    type OnTryRemoveReceivedMessageError =
        | MessageNotFoundErr of MessageId


    type OnTryProcessMessageError =
        | OnTryProcessMessageExn of exn


    type MessagingClientError =
        | GetVersionErr of GetVersionError
        | SendMessageErr of SendMessageError
        | TryReceiveSingleMessageErr of TryReceiveSingleMessageError
        | MessageDeliveryErr of MessageDeliveryError
        | OnTryRemoveReceivedMessageErr of OnTryRemoveReceivedMessageError
        | OnTryProcessMessageErr of OnTryProcessMessageError
