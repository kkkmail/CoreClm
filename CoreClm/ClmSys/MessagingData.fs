namespace ClmSys

open System
open Softellect.Sys.Core
open Softellect.Sys.MessagingPrimitives

open GeneralData
open MessagingPrimitives
open ClmSys.ClmErrors
open ClmSys.MessagingServiceErrors

module MessagingData =

    [<Literal>]
    let MsgDatabase = "MsgClient.db"


    type MessagingServiceAccessInfo =
        {
            messagingServiceAddress : MessagingServiceAddress
            messagingServicePort : MessagingServicePort
            messagingServiceName : MessagingServiceName
        }

        member private s.serviceName = s.messagingServiceName.value.value
        member s.wcfServiceName = toValidServiceName s.serviceName
        member s.wcfServiceUrl = getWcfServiceUrlImpl s.messagingServiceAddress.value s.messagingServicePort.value s.wcfServiceName


    type MessagingClientAccessInfo =
        {
            msgClientId : MessagingClientId
            msgSvcAccessInfo : MessagingServiceAccessInfo
        }

    type MessagingInfo =
        {
            expirationTime : TimeSpan
        }


    type MsgSettings =
        {
            messagingInfo : MessagingInfo
            messagingSvcInfo : MessagingServiceAccessInfo
        }

        member w.isValid() =
            let r =
                [
                    w.messagingSvcInfo.messagingServiceAddress.value.value <> EmptyString, sprintf "%A is invalid" w.messagingSvcInfo.messagingServiceAddress
                    w.messagingSvcInfo.messagingServicePort.value.value > 0, sprintf "%A is invalid" w.messagingSvcInfo.messagingServicePort
                ]
                |> List.fold(fun acc r -> combine acc r) (true, EmptyString)

            match r with
            | true, _ -> Ok()
            | false, s -> s |> InvalidSettings |> MsgSettingsErr |> MessagingServiceErr |> Error


    /// Currently they are the same but this may change.
    type MessagingServiceInfo = MsgSettings
