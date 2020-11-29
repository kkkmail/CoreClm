namespace ClmSys

open GeneralErrors

module ContGenErrors =

    type TryCancelRunQueueError =
        | TryCancelRunQueueWcfErr of WcfError


    type TryRequestResultsError =
        | TryRequestResultsWcfErr of WcfError
        
        
    type ContGenSettingsError =
        | InvalidSettings of string
        | ContGenSettingExn of exn


    type ContGenServiceError =
        | TryCancelRunQueueErr of TryCancelRunQueueError
        | TryRequestResultsErr of TryRequestResultsError
        | ContGenSettingsErr of ContGenSettingsError
        