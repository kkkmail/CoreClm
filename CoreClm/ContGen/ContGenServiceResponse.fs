namespace ContGen

open Softellect.Wcf.Client

open ContGenServiceInfo.ServiceInfo
open ClmSys.ClmErrors
open ClmSys.ContGenData
open ClmSys.ContGenErrors

module ContGenServiceResponse =

    /// Low level WCF ContGen client.
    type ContGenResponseHandler private (url, communicationType, securityMode) =
        let tryGetWcfService() = tryGetWcfService<IContGenWcfService> communicationType securityMode url
        let toCancelRunQueueError f = f |> TryCancelRunQueueWcfErr |> TryCancelRunQueueErr |> ContGenServiceErr
        let toRequestResultsError f = f |> TryRequestResultsWcfErr |> TryRequestResultsErr |> ContGenServiceErr
        let toResetError f = f |> TryResetWcfErr |> TryResetErr |> ContGenServiceErr
        let tryCancelRunQueueImpl q c = tryCommunicate tryGetWcfService (fun service -> service.tryCancelRunQueue) toCancelRunQueueError (q, c)
        let tryRequestResultsImpl q c = tryCommunicate tryGetWcfService (fun service -> service.tryRequestResults) toRequestResultsError (q, c)
        let tryResetImpl q = tryCommunicate tryGetWcfService (fun service -> service.tryReset) toResetError q

        interface IContGenService with
            member _.tryCancelRunQueue q c = tryCancelRunQueueImpl q c
            member _.tryRequestResults q c = tryRequestResultsImpl q c
            member _.tryReset q = tryResetImpl q

        new (i : ContGenServiceAccessInfo, communicationType, securityMode) =
            ContGenResponseHandler(i.value.getUrl communicationType, communicationType, securityMode)
