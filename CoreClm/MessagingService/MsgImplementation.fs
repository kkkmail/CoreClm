namespace MessagingService

open System.ServiceModel
open ClmSys.Logging
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open MessagingService.SvcCommandLine
open Messaging.Service
open ServiceProxy.MsgServiceProxy
open ClmSys.Wcf
open ClmSys.MessagingServiceErrors
open ClmSys.ClmErrors
open DbData.Configuration
open ClmSys

module ServiceImplementation =

    let mutable serviceAccessInfo = getServiceAccessInfo []


    let private createMessagingService logger (i : MessagingServiceInfo) : MessagingService =
        let d : MessagingServiceData =
            {
                messagingServiceProxy = MessagingServiceProxy.create getMsgSvcConnectionString
                expirationTime = i.messagingInfo.expirationTime
            }

        let service = MessagingService d
        createMessagingServiceEventHandlers logger service
        service


    let private messagingService = new Lazy<ClmResult<MessagingService>>(fun () -> createMessagingService Logger.log4net serviceAccessInfo |> Ok)


    [<ServiceBehavior(IncludeExceptionDetailInFaults = true, InstanceContextMode = InstanceContextMode.PerSession)>]
    type MessagingWcfService() =
        let toGetVersionError f = f |> GetVersionSvcWcfErr |> GetVersionSvcErr |> MessagingServiceErr
        let toSendMessageError f = f |> MsgWcfErr |> MessageDeliveryErr |> MessagingServiceErr
        let toTryPickMessageError f = f |> TryPeekMsgWcfErr |> TryPeekMessageErr |> MessagingServiceErr
        let toTryDeleteFromServerError f = f |> TryDeleteMsgWcfErr |> TryDeleteFromServerErr |> MessagingServiceErr

        let getVersion() = messagingService.Value |> Rop.bind (fun e -> e.getVersion())
        let sendMessage b = messagingService.Value |> Rop.bind (fun e -> e.sendMessage b)
        let tryPeekMessage b = messagingService.Value |> Rop.bind (fun e -> e.tryPeekMessage b)
        let tryDeleteFromServer b = messagingService.Value |> Rop.bind (fun e -> e.tryDeleteFromServer b)

        interface IMessagingWcfService with
            member _.getVersion b = tryReply getVersion toGetVersionError b
            member _.sendMessage b = tryReply sendMessage toSendMessageError b
            member _.tryPeekMessage b = tryReply tryPeekMessage toTryPickMessageError b
            member _.tryDeleteFromServer b = tryReply tryDeleteFromServer toTryDeleteFromServerError b
