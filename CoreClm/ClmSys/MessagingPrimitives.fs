﻿namespace ClmSys

open System
open GeneralPrimitives

module MessagingPrimitives =

    type MessagingClientName =
        | MessagingClientName of string

        member this.value = let (MessagingClientName v) = this in v


    type ServiceName
        with
        member n.messagingClientName = MessagingClientName n.originalValue


    type MessagingServiceAddress =
        | MessagingServiceAddress of ServiceAddress

        member this.value = let (MessagingServiceAddress v) = this in v
        static member defaultValue = DefaultMessagingServiceAddress |> ServiceAddress |> MessagingServiceAddress


    type MessagingServicePort =
        | MessagingServicePort of ServicePort

        member this.value = let (MessagingServicePort v) = this in v
        static member defaultValue = DefaultMessagingServicePort |> ServicePort |> MessagingServicePort


    type MessagingServiceName =
        | MessagingServiceName of ServiceName

        member this.value = let (MessagingServiceName v) = this in v


    let messagingServiceName = "MessagingService" |> ServiceName |> MessagingServiceName
