namespace ContGen

open Softellect.Wcf.Common
open Softellect.Wcf.Client

open ContGenServiceInfo.ServiceInfo
open ClmSys.ClmErrors
open ClmSys.ContGenData
open ClmSys.ContGenErrors

module ContGenServiceResponse =

    /// Low level WCF messaging client.
    type ContGenResponseHandler private (url) =
        let tryGetWcfService() = tryGetWcfService<IContGenWcfService> url
        let toCancelRunQueueError f = f |> TryCancelRunQueueWcfErr |> TryCancelRunQueueErr |> ContGenServiceErr
        let toRequestResultsError f = f |> TryRequestResultsWcfErr |> TryRequestResultsErr |> ContGenServiceErr
        let tryCancelRunQueueImpl q c = tryCommunicate tryGetWcfService (fun service -> service.tryCancelRunQueue) toCancelRunQueueError (q, c)
        let tryRequestResultsImpl q c = tryCommunicate tryGetWcfService (fun service -> service.tryRequestResults) toRequestResultsError (q, c)

        interface IContGenService with
            member _.tryCancelRunQueue q c = tryCancelRunQueueImpl q c
            member _.tryRequestResults q c = tryRequestResultsImpl q c

        new (i : ContGenServiceAccessInfo) = ContGenResponseHandler(i.wcfServiceUrl)
