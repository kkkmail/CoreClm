namespace ClmSys

open Softellect.Sys.Core
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.ServiceInstaller
open Softellect.Sys.Primitives
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.MessagingServiceErrors
open Softellect.Messaging.ServiceInfo

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
