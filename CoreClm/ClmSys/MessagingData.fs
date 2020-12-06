﻿namespace ClmSys

open System

open Softellect.Sys.Core
open Softellect.Wcf.Common
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.MessagingServiceErrors
open Softellect.Messaging.ServiceInfo
open Softellect.Messaging.Service

open GeneralData
open MessagingPrimitives
open ClmSys.ClmErrors

module MessagingData =

    [<Literal>]
    let MsgDatabase = "MsgClient.db"


    //type MessagingServiceAccessInfo =
    //    {
    //        messagingServiceAddress : MessagingServiceAddress
    //        messagingServicePort : MessagingServicePort
    //        messagingServiceName : MessagingServiceName
    //    }

    //    member private s.serviceName = s.messagingServiceName.value.value
    //    member s.wcfServiceName = toValidServiceName s.serviceName
    //    member s.wcfServiceUrl = getWcfServiceUrlImpl s.messagingServiceAddress.value s.messagingServicePort.value s.wcfServiceName


    //type MessagingClientAccessInfo =
    //    {
    //        msgClientId : MessagingClientId
    //        msgSvcAccessInfo : MessagingServiceAccessInfo
    //    }

    //type MessagingInfo =
    //    {
    //        expirationTime : TimeSpan
    //    }


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
                    h.httpServiceAddress.value <> EmptyString, sprintf "%A is invalid" h.httpServiceAddress
                    h.httpServicePort.value > 0, sprintf "%A is invalid" h.httpServicePort.value
                ]
                |> List.fold(fun acc r -> combine acc r) (true, EmptyString)

            match r with
            | true, _ -> Ok()
            | false, s -> s |> InvalidSettings |> MsgSettingsErr |> MessagingServiceErr |> Error
