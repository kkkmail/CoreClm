namespace Messaging

open System
open ClmSys.TimerEvents
open ClmSys.VersionInfo
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
open ClmSys.ClmErrors
open ClmSys.MessagingPrimitives

module Service =
    let x = 1

//    type MessagingServiceData =
//        {
//            messagingServiceProxy : MessagingServiceProxy
//            expirationTime : TimeSpan
//        }
//
//        static member defaultExpirationTime = TimeSpan.FromMinutes 5.0
//
//    type MessagingService(d : MessagingServiceData) =
//        let proxy = d.messagingServiceProxy
//        member _.getVersion() : ClmResult<MessagingDataVersion> = Ok messagingDataVersion
//
//        member _.sendMessage (m : Message) : UnitResult =
//            let result = proxy.saveMessage m
//
//            match result with
//            | Ok() -> ()
//            | Error e -> printfn "ERROR - MessagingService.sendMessage failed for messageId: %A with error: %A" m.messageDataInfo.messageId e
//            result
//
//        member _.tryPeekMessage (n : MessagingClientId) : ClmResult<Message option> = proxy.tryPickMessage n
//        member _.tryDeleteFromServer (n : MessagingClientId, m : MessageId) : UnitResult = proxy.deleteMessage m
//        member _.removeExpiredMessages() : UnitResult = proxy.deleteExpiredMessages d.expirationTime
//
//
//    /// Call this function to create timer events necessary for automatic MessagingService operation.
//    /// If you don't call it, then you have to operate MessagingService by hands.
//    let createMessagingServiceEventHandlers logger (w : MessagingService) =
//        let eventHandler _ = w.removeExpiredMessages()
//        let h = ClmEventHandlerInfo.defaultValue logger eventHandler "MessagingService - removeExpiredMessages" |> ClmEventHandler
//        do h.start()

