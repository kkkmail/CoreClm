namespace ServiceProxy

open Softellect.Messaging.Proxy

open MessagingServiceInfo.ServiceInfo
open ClmSys.ClmErrors

module MsgProcessorProxy =

    type MessageProcessorProxy = MessageProcessorProxy<ClmMessageData, ClmError>
    type OnProcessMessageType<'S> = OnProcessMessageType<'S, ClmMessageData, ClmError>
    type OnGetMessagesProxy<'S> = OnGetMessagesProxy<'S, ClmMessageData, ClmError>
    let onGetMessages<'S> proxy s = onGetMessages<'S, ClmMessageData, ClmError> proxy s
