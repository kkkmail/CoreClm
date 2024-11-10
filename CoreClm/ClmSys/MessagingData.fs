namespace ClmSys

open Softellect.Sys.Core
open Softellect.Wcf.Common
open Softellect.Messaging.Errors
open Softellect.Messaging.ServiceInfo
open Softellect.Messaging.Service
open Primitives.GeneralData
open ClmSys.ClmErrors
open Softellect.Sys.Primitives

module MessagingData =

    [<Literal>]
    let MsgDatabase = "MsgClient.db"


    type MsgSettings =
        {
            messagingInfo : MessagingServiceInfo
            messagingSvcInfo : MessagingServiceAccessInfo
            communicationType : WcfCommunicationType
        }

        member w.isValid() =
            let h = w.messagingSvcInfo.messagingServiceAccessInfo.httpServiceInfo

            let r =
                [
                    h.httpServiceAddress.value <> EmptyString, $"%A{h.httpServiceAddress} is invalid"
                    h.httpServicePort.value > 0, $"%A{h.httpServicePort.value} is invalid"
                ]
                |> List.fold(fun acc r -> combine acc r) (true, EmptyString)

            match r with
            | true, _ -> Ok()
            | false, s -> s |> InvalidSettings |> Error
