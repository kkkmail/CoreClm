namespace ClmSys
open Softellect.Sys.WcfErrors

module ContGenErrors =

    type TryCancelRunQueueError =
        | TryCancelRunQueueWcfErr of WcfError


    type TryRequestResultsError =
        | TryRequestResultsWcfErr of WcfError


    type TryResetError =
        | TryResetWcfErr of WcfError


    type ContGenSettingsError =
        | InvalidSettings of string
        | ContGenSettingExn of exn


    type ContGenServiceError =
        | TryCancelRunQueueErr of TryCancelRunQueueError
        | TryRequestResultsErr of TryRequestResultsError
        | TryResetErr of TryResetError
        | ContGenSettingsErr of ContGenSettingsError
