namespace ClmSys

open Softellect.Sys.Primitives
open Softellect.Messaging.Primitives

module MessagingPrimitives =

    let messagingHttpServiceName = "MessagingHttpService" |> ServiceName |> MessagingServiceName
    let messagingNetTcpServiceName = "MessagingNetTcpService" |> ServiceName |> MessagingServiceName
