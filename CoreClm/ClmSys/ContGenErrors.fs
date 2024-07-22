namespace ClmSys
open Softellect.Wcf.Errors
open Softellect.Messaging.Errors

module ContGenErrors =
    let x = 1

    type ContGenServiceError =
        | TryCancelRunQueueWcfErr of WcfError
        | TryRequestResultsWcfErr of WcfError
        | TryResetWcfErr of WcfError

        // ======
        // Ugly stuff
        // TODO kk:20240722 - WorkNode error types are now inconsistent and conflict with messaging error types.
        // See: https://github.com/kkkmail/CoreClm/issues/40
        | TryCreateModelRunnerErr of MessagingError


    //type TryCancelRunQueueError =
    //    | TryCancelRunQueueWcfErr of WcfError


    //type TryRequestResultsError =
    //    | TryRequestResultsWcfErr of WcfError


    //type TryResetError =
    //    | TryResetWcfErr of WcfError


    //type ContGenSettingsError =
    //    | InvalidSettings of string
    //    | ContGenSettingExn of exn


    //type ContGenServiceError =
    //    | TryCancelRunQueueErr of TryCancelRunQueueError
    //    | TryRequestResultsErr of TryRequestResultsError
    //    | TryResetErr of TryResetError
    //    | ContGenSettingsErr of ContGenSettingsError
