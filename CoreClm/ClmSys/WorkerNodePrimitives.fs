namespace ClmSys

open GeneralPrimitives
open MessagingPrimitives

module WorkerNodePrimitives =

    type WorkerNodeServiceAddress =
        | WorkerNodeServiceAddress of ServiceAddress

        member this.value = let (WorkerNodeServiceAddress v) = this in v
        static member defaultValue = DefaultWorkerNodeServiceAddress |> ServiceAddress |> WorkerNodeServiceAddress


    type WorkerNodeServicePort =
        | WorkerNodeServicePort of ServicePort

        member this.value = let (WorkerNodeServicePort v) = this in v
        static member defaultValue = DefaultWorkerNodeServicePort |> ServicePort |> WorkerNodeServicePort


    type WorkerNodeServiceName =
        | WorkerNodeServiceName of ServiceName

        member this.value = let (WorkerNodeServiceName v) = this in v


    let workerNodeServiceName = "WorkerNodeService" |> ServiceName |> WorkerNodeServiceName


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
