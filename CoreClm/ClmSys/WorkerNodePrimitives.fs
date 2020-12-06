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

    type WorkerNodeServiceAddress =
        | WorkerNodeServiceAddress of ServiceAddress

        member this.value = let (WorkerNodeServiceAddress v) = this in v
        static member defaultValue = defaultWorkerNodeServiceAddress |> ServiceAddress |> WorkerNodeServiceAddress


    type WorkerNodeServicePort =
        | WorkerNodeServicePort of ServicePort

        member this.value = let (WorkerNodeServicePort v) = this in v
        static member defaultNetTcpValue = defaultWorkerNodeNetTcpServicePort |> ServicePort |> WorkerNodeServicePort
        static member defaultHttpValue = defaultWorkerNodeHttpServicePort |> ServicePort |> WorkerNodeServicePort


    type WorkerNodeServiceName =
        | WorkerNodeServiceName of ServiceName

        member this.value = let (WorkerNodeServiceName v) = this in v


    let workerNodeNetTcpServiceName = "WorkerNodeNetTcpService" |> ServiceName |> WorkerNodeServiceName
    let workerNodeHttpServiceName = "WorkerNodeHttpService" |> ServiceName |> WorkerNodeServiceName
    let workerNodeServiceName = workerNodeNetTcpServiceName


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
