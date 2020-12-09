namespace MessagingService

open Softellect.Sys.Primitives
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.Logging
open Softellect.Sys.MessagingErrors
open Softellect.Wcf.Common
open Softellect.Wcf.Service
open Softellect.Messaging.Primitives
open Softellect.Messaging.ServiceInfo
open Softellect.Messaging.Service
open Softellect.Sys.WcfErrors
open Softellect.Messaging.Client
open Softellect.Messaging.Proxy

open System.ServiceModel
open ClmSys.Logging
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open MessagingService.SvcCommandLine
open Messaging.Service
open ServiceProxy.MsgServiceProxy
open ClmSys.ClmErrors
open DbData.Configuration
open ClmSys.VersionInfo

module ServiceImplementation =

    let mutable serviceSettings = getServiceSettings []


    let tryCreateMessagingServiceData logger : Result<MessagingWcfServiceData, WcfError> =
        let i = getServiceSettings []

        let serviceData : MessagingServiceData =
            {
                messagingServiceInfo =
                    {
                        expirationTime = i.messagingInfo.expirationTime
                        messagingDataVersion = messagingDataVersion
                    }

                messagingServiceProxy = createMessagingServiceProxy getMsgSvcConnectionString
                communicationType = i.communicationType
            }

        let msgServiceDataRes = tryGetMsgServiceData i.messagingSvcInfo.messagingServiceAccessInfo logger serviceData
        msgServiceDataRes


    //let tryRunMessagingService dr =
    //    match dr with
    //    | Ok data ->
    //        match MessagingWcfServiceImpl.tryGetService data with
    //        | Ok host ->
    //            match host.run() with
    //            | Ok() -> Ok host
    //            | Error e -> Error e
    //        | Error e -> Error e
    //    | Error e -> Error e


    let messagingServiceData = Lazy<Result<MessagingWcfServiceData, WcfError>>(fun () -> tryCreateMessagingServiceData (failwith ""))


    //let private messagingService = new Lazy<ClmResult<MessagingService>>(fun () -> createMessagingService Logger.log4net serviceAccessInfo |> Ok)


    //[<ServiceBehavior(IncludeExceptionDetailInFaults = true, InstanceContextMode = InstanceContextMode.PerSession)>]
    //type MessagingWcfService() =
    //    let toGetVersionError f = f |> GetVersionSvcWcfErr |> GetVersionSvcErr |> MessagingServiceErr
    //    let toSendMessageError f = f |> MsgWcfErr |> MessageDeliveryErr |> MessagingServiceErr
    //    let toTryPickMessageError f = f |> TryPeekMsgWcfErr |> TryPeekMessageErr |> MessagingServiceErr
    //    let toTryDeleteFromServerError f = f |> TryDeleteMsgWcfErr |> TryDeleteFromServerErr |> MessagingServiceErr

    //    let getVersion() = messagingService.Value |> Rop.bind (fun e -> e.getVersion())
    //    let sendMessage b = messagingService.Value |> Rop.bind (fun e -> e.sendMessage b)
    //    let tryPeekMessage b = messagingService.Value |> Rop.bind (fun e -> e.tryPeekMessage b)
    //    let tryDeleteFromServer b = messagingService.Value |> Rop.bind (fun e -> e.tryDeleteFromServer b)

    //    interface IMessagingWcfService with
    //        member _.getVersion b = tryReply getVersion toGetVersionError b
    //        member _.sendMessage b = tryReply sendMessage toSendMessageError b
    //        member _.tryPeekMessage b = tryReply tryPeekMessage toTryPickMessageError b
    //        member _.tryDeleteFromServer b = tryReply tryDeleteFromServer toTryDeleteFromServerError b
