namespace ClmSys

open System

open Softellect.Sys.Core
open Softellect.Wcf.Common
open Softellect.Messaging.ServiceInfo

open ClmSys.GeneralData
open ClmSys.WorkerNodePrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.WorkerNodeErrors
open ClmSys.ClmErrors

module WorkerNodeData =

    type WorkerNodeInfo =
        {
            workerNodeId : WorkerNodeId
            workerNodeName : WorkerNodeName
            partitionerId : PartitionerId
            noOfCores : int
            nodePriority : WorkerNodePriority
            isInactive : bool
            lastErrorDateOpt : DateTime option
        }


    type WorkerNodeServiceAccessInfo =
        | WorkerNodeServiceAccessInfo of ServiceAccessInfo

        member w.value = let (WorkerNodeServiceAccessInfo v) = w in v

        static member create address httpPort netTcpPort =
            let h = HttpServiceAccessInfo.create address httpPort WorkerNodeServiceName.httpServiceName.value
            let n = NetTcpServiceAccessInfo.create address netTcpPort WorkerNodeServiceName.netTcpServiceName.value
            ServiceAccessInfo.create h n |> WorkerNodeServiceAccessInfo


    type WorkerNodeServiceInfo =
        {
            workerNodeInfo : WorkerNodeInfo
            workerNodeServiceAccessInfo : WorkerNodeServiceAccessInfo
            messagingServiceAccessInfo : MessagingServiceAccessInfo
        }

        member this.messagingClientAccessInfo =
            {
                msgClientId = this.workerNodeInfo.workerNodeId.messagingClientId
                msgSvcAccessInfo = this.messagingServiceAccessInfo
            }


    type WorkerNodeSettings =
        {
            workerNodeInfo : WorkerNodeInfo
            workerNodeSvcInfo : WorkerNodeServiceAccessInfo
            workerNodeCommunicationType : WcfCommunicationType
            messagingSvcInfo : MessagingServiceAccessInfo
            messagingCommunicationType : WcfCommunicationType
        }

        member w.isValid() =
            let r =
                [
                    w.workerNodeInfo.workerNodeName.value <> EmptyString, sprintf "%A is invalid" w.workerNodeInfo.workerNodeName
                    w.workerNodeInfo.workerNodeId.value.value <> Guid.Empty, sprintf "%A is invalid" w.workerNodeInfo.workerNodeId
                    w.workerNodeInfo.noOfCores >= 0, sprintf "noOfCores: %A is invalid" w.workerNodeInfo.noOfCores
                    w.workerNodeInfo.partitionerId.value.value <> Guid.Empty, sprintf "%A is invalid" w.workerNodeInfo.partitionerId

//                    w.workerNodeSvcInfo.workerNodeServiceAddress.value.value <> EmptyString, sprintf "%A is invalid" w.workerNodeSvcInfo.workerNodeServiceAddress
//                    w.workerNodeSvcInfo.workerNodeServicePort.value.value > 0, sprintf "%A is invalid" w.workerNodeSvcInfo.workerNodeServicePort
//
//                    w.messagingSvcInfo.messagingServiceAddress.value.value <> EmptyString, sprintf "%A is invalid" w.messagingSvcInfo.messagingServiceAddress
//                    w.messagingSvcInfo.messagingServicePort.value.value > 0, sprintf "%A is invalid" w.messagingSvcInfo.messagingServicePort
                ]
                |> List.fold(fun acc r -> combine acc r) (true, EmptyString)

            match r with
            | true, _ -> Ok()
            | false, s -> s |> InvalidSettings |> WrkSettingsErr |> WorkerNodeErr |> Error
