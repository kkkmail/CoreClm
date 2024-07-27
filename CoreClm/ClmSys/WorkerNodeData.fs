namespace ClmSys

open System

open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives
open Softellect.Sys.Core
open Softellect.Sys.Primitives
open Softellect.Wcf.Common
open Softellect.Messaging.ServiceInfo
open ClmSys.WorkerNodePrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.WorkerNodeErrors
open ClmSys.ClmErrors
open Primitives.GeneralData

module WorkerNodeData =
    let 

//    type WorkerNodeInfo =
//        {
//            workerNodeId : WorkerNodeId
//            workerNodeName : WorkerNodeName
//            partitionerId : PartitionerId
//            noOfCores : int
//            nodePriority : WorkerNodePriority
//            isInactive : bool
//            lastErrorDateOpt : DateTime option
//        }


//    type WorkerNodeServiceAccessInfo =
//        | WorkerNodeServiceAccessInfo of ServiceAccessInfo

//        member w.value = let (WorkerNodeServiceAccessInfo v) = w in v

//        static member create address httpPort netTcpPort securityMode =
//            let h = HttpServiceAccessInfo.create address httpPort WorkerNodeServiceName.httpServiceName.value
//            let n = NetTcpServiceAccessInfo.create address netTcpPort WorkerNodeServiceName.netTcpServiceName.value securityMode
//            ServiceAccessInfo.create h n |> WorkerNodeServiceAccessInfo


//    type WorkerNodeServiceInfo =
//        {
//            workerNodeInfo : WorkerNodeInfo
//            workerNodeServiceAccessInfo : WorkerNodeServiceAccessInfo
//            messagingServiceAccessInfo : MessagingServiceAccessInfo
//        }

//        member this.messagingClientAccessInfo =
//            {
//                msgClientId = this.workerNodeInfo.workerNodeId.messagingClientId
//                msgSvcAccessInfo = this.messagingServiceAccessInfo
//            }


//    type WorkerNodeSettings =
//        {
//            workerNodeInfo : WorkerNodeInfo
//            workerNodeSvcInfo : WorkerNodeServiceAccessInfo
//            workerNodeCommunicationType : WcfCommunicationType
//            messagingSvcInfo : MessagingServiceAccessInfo
//            messagingCommunicationType : WcfCommunicationType
//        }

//        member w.isValid() =
//            let r =
//                [
//                    w.workerNodeInfo.workerNodeName.value <> EmptyString, $"%A{w.workerNodeInfo.workerNodeName} is invalid"
//                    w.workerNodeInfo.workerNodeId.value.value <> Guid.Empty, $"%A{w.workerNodeInfo.workerNodeId} is invalid"
//                    w.workerNodeInfo.noOfCores >= 0, $"noOfCores: %A{w.workerNodeInfo.noOfCores} is invalid"
//                    w.workerNodeInfo.partitionerId.value.value <> Guid.Empty, $"%A{w.workerNodeInfo.partitionerId} is invalid"

////                    w.workerNodeSvcInfo.workerNodeServiceAddress.value.value <> EmptyString, sprintf "%A is invalid" w.workerNodeSvcInfo.workerNodeServiceAddress
////                    w.workerNodeSvcInfo.workerNodeServicePort.value.value > 0, sprintf "%A is invalid" w.workerNodeSvcInfo.workerNodeServicePort
////
////                    w.messagingSvcInfo.messagingServiceAddress.value.value <> EmptyString, sprintf "%A is invalid" w.messagingSvcInfo.messagingServiceAddress
////                    w.messagingSvcInfo.messagingServicePort.value.value > 0, sprintf "%A is invalid" w.messagingSvcInfo.messagingServicePort
//                ]
//                |> List.fold(fun acc r -> combine acc r) (true, EmptyString)

//            match r with
//            | true, _ -> Ok()
//            | false, s -> s |> InvalidSettings |> Error


    //type SolverRunnerInfo =
    //    {
    //        runQueueId : RunQueueId
    //        processId : ProcessId option
    //    }
