namespace ClmSys

open Softellect.Messaging.Primitives

module PartitionerPrimitives =

    type PartitionerId =
        | PartitionerId of MessagingClientId

        member this.value = let (PartitionerId v) = this in v
        member this.messagingClientId = let (PartitionerId v) = this in v
