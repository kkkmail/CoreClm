namespace ServiceProxy

open Softellect.Messaging.Proxy

open MessagingServiceInfo.ServiceInfo
open ClmSys.ClmErrors

module MsgProcessorProxy =

    type MessageProcessorProxy = MessageProcessorProxy<ClmMessageData>
    type OnProcessMessageType<'S> = OnProcessMessageType<'S, ClmMessageData>
    //type OnGetMessagesProxy<'S> = OnGetMessagesProxy<'S, ClmMessageData>
    let onGetMessages<'S> proxy s = onGetMessages<'S, ClmMessageData> proxy s
