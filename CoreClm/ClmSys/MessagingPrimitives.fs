namespace ClmSys

open System
open Softellect.Sys.Primitives
open Softellect.Messaging.Primitives
open GeneralPrimitives

module MessagingPrimitives =
    let x = 1

//    type ServiceName
//        with
//        member n.messagingClientName = MessagingClientName n.value
//
//
//    type MessagingServiceAddress =
//        | MessagingServiceAddress of ServiceAddress
//
//        member this.value = let (MessagingServiceAddress v) = this in v
//        static member defaultValue = DefaultMessagingServiceAddress |> ServiceAddress |> MessagingServiceAddress
//
//
//    type MessagingServicePort =
//        | MessagingServicePort of ServicePort
//
//        member this.value = let (MessagingServicePort v) = this in v
//        static member defaultValue = DefaultMessagingServicePort |> ServicePort |> MessagingServicePort


    let messagingHttpServiceName = "MessagingHttpService" |> ServiceName |> MessagingServiceName
    let messagingNetTcpServiceName = "MessagingNetTcpService" |> ServiceName |> MessagingServiceName
