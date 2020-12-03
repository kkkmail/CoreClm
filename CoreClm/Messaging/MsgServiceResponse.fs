namespace Messaging

open Softellect.Sys.Primitives
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.Logging
open Softellect.Sys.MessagingErrors
open Softellect.Wcf.Common
open Softellect.Wcf.Service
open Softellect.Messaging.Primitives
open Softellect.Messaging.ServiceInfo
open Softellect.Messaging.Service
open Softellect.Messaging.Client
open Softellect.Messaging.Proxy

open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open ClmSys.MessagingServiceErrors
open ClmSys.ClmErrors

module ServiceResponse =
    let x = 1

//    type MessagingClient = MessagingClient<ClmMessageData, ClmError>
//    type MessagingClientData = MessagingClientData<ClmMessageData, ClmError>
//    type MessagingServiceData = MessagingServiceData<ClmMessageData, ClmError>
//    type Message = Message<ClmMessageData>
//    type MessagingService = MessagingService<ClmMessageData, ClmError>
//    type MessagingWcfService = MessagingWcfService<ClmMessageData, ClmError>
//    type MessagingWcfServiceImpl = WcfService<MessagingWcfService, IMessagingWcfService, MessagingServiceData>

//    type MsgResponseHandler private (url) =
//        let tryGetWcfService() = tryGetWcfService<IMessagingWcfService> url
//
//        let getVersionWcfErr e = e |> GetVersionSvcWcfErr |> GetVersionSvcErr |> MessagingServiceErr
//        let msgWcfErr e = e |> MsgWcfErr |> MessageDeliveryErr |> MessagingServiceErr
//        let tryPeekMsgWcfErr e = e |> TryPeekMsgWcfErr |> TryPeekMessageErr |> MessagingServiceErr
//        let tryDeleteMsgWcfErr e = e |> TryDeleteMsgWcfErr |> TryDeleteFromServerErr |> MessagingServiceErr
//
//        let getVersionImpl() = tryCommunicate tryGetWcfService (fun service -> service.getVersion) getVersionWcfErr ()
//        let sendMessageImpl m = tryCommunicate tryGetWcfService (fun service -> service.sendMessage) msgWcfErr m
//        let tryPeekMessageImpl n = tryCommunicate tryGetWcfService (fun service -> service.tryPeekMessage) tryPeekMsgWcfErr n
//        let tryDeleteFromServerImpl x = tryCommunicate tryGetWcfService (fun service -> service.tryDeleteFromServer) tryDeleteMsgWcfErr x
//
//        interface IMessagingService with
//            member _.getVersion() = getVersionImpl()
//            member _.sendMessage m = sendMessageImpl m
//            member _.tryPeekMessage n = tryPeekMessageImpl n
//            member _.tryDeleteFromServer x = tryDeleteFromServerImpl x
//
//        new (i : MessagingClientAccessInfo) = MsgResponseHandler(i.msgSvcAccessInfo.wcfServiceUrl)
//        new (i : MessagingServiceAccessInfo) = MsgResponseHandler(i.wcfServiceUrl)
