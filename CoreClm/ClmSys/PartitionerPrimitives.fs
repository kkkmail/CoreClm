namespace ClmSys

open MessagingPrimitives

module PartitionerPrimitives =

    type PartitionerId =
        | PartitionerId of MessagingClientId

        member this.value = let (PartitionerId v) = this in v
        member this.messagingClientId = let (PartitionerId v) = this in v
