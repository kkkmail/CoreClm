namespace ClmSys

open System

open Softellect.Sys.Core
open Softellect.Sys.ServiceInstaller
open Softellect.Sys.Primitives
open Softellect.Sys.Core
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.MessagingServiceErrors
open Softellect.Messaging.ServiceInfo

open ClmSys.GeneralData
open ClmSys.MessagingData
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
        {
            workerNodeServiceAddress : WorkerNodeServiceAddress
            workerNodeServicePort : WorkerNodeServicePort
            workerNodeServiceName : WorkerNodeServiceName
        }

        //member s.serviceName = s.workerNodeServiceName.value.value
        //member s.serviceUrl = getServiceUrlImpl s.workerNodeServiceAddress.value s.workerNodeServicePort.value s.serviceName
        //member s.wcfServiceName = toValidServiceName s.serviceName
        //member s.wcfServiceUrl = getWcfServiceUrlImpl s.workerNodeServiceAddress.value s.workerNodeServicePort.value s.wcfServiceName


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
            messagingSvcInfo : MessagingServiceAccessInfo
        }

        member w.isValid() =
            let r =
                [
                    w.workerNodeInfo.workerNodeName.value <> EmptyString, sprintf "%A is invalid" w.workerNodeInfo.workerNodeName
                    w.workerNodeInfo.workerNodeId.value.value <> Guid.Empty, sprintf "%A is invalid" w.workerNodeInfo.workerNodeId
                    w.workerNodeInfo.noOfCores >= 0, sprintf "noOfCores: %A is invalid" w.workerNodeInfo.noOfCores
                    w.workerNodeInfo.partitionerId.value.value <> Guid.Empty, sprintf "%A is invalid" w.workerNodeInfo.partitionerId

                    w.workerNodeSvcInfo.workerNodeServiceAddress.value.value <> EmptyString, sprintf "%A is invalid" w.workerNodeSvcInfo.workerNodeServiceAddress
                    w.workerNodeSvcInfo.workerNodeServicePort.value.value > 0, sprintf "%A is invalid" w.workerNodeSvcInfo.workerNodeServicePort

                    //w.messagingSvcInfo.messagingServiceAddress.value.value <> EmptyString, sprintf "%A is invalid" w.messagingSvcInfo.messagingServiceAddress
                    //w.messagingSvcInfo.messagingServicePort.value.value > 0, sprintf "%A is invalid" w.messagingSvcInfo.messagingServicePort
                ]
                |> List.fold(fun acc r -> combine acc r) (true, EmptyString)

            match r with
            | true, _ -> Ok()
            | false, s -> s |> InvalidSettings |> WrkSettingsErr |> WorkerNodeErr |> Error
