namespace ClmSys

open Softellect.Sys.MessagingPrimitives

open System
open MessagingData
open PartitionerPrimitives

module PartitionerData =

    let defaultPartitionerId = Guid("F941F87C-BEBC-43E7-ABD3-967E377CBD57") |> MessagingClientId |> PartitionerId


    /// Partitioner MessagingClientId + Messaging Service access info.
    type PartitionerMsgAccessInfo =
        {
            partitionerId : PartitionerId
            messagingServiceAccessInfo : MessagingServiceAccessInfo
        }

        member this.messagingClientAccessInfo =
            {
                msgClientId = this.partitionerId.messagingClientId
                msgSvcAccessInfo = this.messagingServiceAccessInfo
            }
