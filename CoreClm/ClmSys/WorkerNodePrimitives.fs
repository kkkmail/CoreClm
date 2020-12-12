namespace ClmSys

open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.ServiceInstaller
open Softellect.Sys.Primitives
open Softellect.Sys.Core
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.MessagingServiceErrors
open Softellect.Messaging.ServiceInfo

open GeneralPrimitives

module WorkerNodePrimitives =

    type WorkerNodeServiceName =
        | WorkerNodeServiceName of ServiceName

        member this.value = let (WorkerNodeServiceName v) = this in v
        static member netTcpServiceName = "WorkerNodeNetTcpService" |> ServiceName |> WorkerNodeServiceName
        static member httpServiceName = "WorkerNodeHttpService" |> ServiceName |> WorkerNodeServiceName


    type WorkerNodeId =
        | WorkerNodeId of MessagingClientId

        member this.value = let (WorkerNodeId v) = this in v
        member this.messagingClientId = let (WorkerNodeId v) = this in v


    type WorkerNodePriority =
        | WorkerNodePriority of int

        member this.value = let (WorkerNodePriority v) = this in v
        static member defaultValue = WorkerNodePriority 100


    type WorkerNodeName =
        | WorkerNodeName of string

        member this.value = let (WorkerNodeName v) = this in v
