namespace ClmSys

open System
open System.ServiceModel

open Softellect.Sys.Core
open Softellect.Sys.ServiceInstaller
open Softellect.Sys.Primitives
open Softellect.Sys.Core
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.MessagingServiceErrors
open Softellect.Messaging.ServiceInfo
open Softellect.Wcf.Common


open ClmSys
open GeneralPrimitives
open GeneralData
open ContGenPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.ClmErrors
open ClmSys.ContGenErrors
open ClmSys.MessagingData

module ContGenData =

    type ContGenServiceAccessInfo =
        | ContGenServiceAccessInfo of ServiceAccessInfo

        member i.value = let (ContGenServiceAccessInfo a) = i in a

        static member create address httpPort netTcpPort =
            let h = HttpServiceAccessInfo.create address httpPort ContGenServiceName.httpServiceName.value
            let n = NetTcpServiceAccessInfo.create address netTcpPort ContGenServiceName.netTcpServiceName.value
            ServiceAccessInfo.create h n |> ContGenServiceAccessInfo


    type ContGenInfo =
        {
            minUsefulEe : MinUsefulEe
            partitionerId : PartitionerId
            lastAllowedNodeErr : LastAllowedNodeErr
            earlyExitCheckFreq : EarlyExitCheckFreq
        }


    type ContGenSettings =
        {
            contGenInfo : ContGenInfo
            contGenSvcInfo : ContGenServiceAccessInfo
            contGenCommType : WcfCommunicationType
            messagingSvcInfo : MessagingServiceAccessInfo
            messagingCommType : WcfCommunicationType
        }

        member c.messagingClientAccessInfo =
            {
                msgClientId = c.contGenInfo.partitionerId.messagingClientId
                msgSvcAccessInfo = c.messagingSvcInfo
            }

        member w.isValid() =
            let r =
                [
                    //w.contGenSvcInfo.contGenServiceAddress.value.value <> EmptyString, sprintf "%A is invalid" w.contGenSvcInfo.contGenServiceAddress
                    //w.contGenSvcInfo.contGenServicePort.value.value > 0, sprintf "%A is invalid" w.contGenSvcInfo.contGenServicePort

                    //w.messagingSvcInfo.messagingServiceAddress.value.value <> EmptyString, sprintf "%A is invalid" w.messagingSvcInfo.messagingServiceAddress
                    //w.messagingSvcInfo.messagingServicePort.value.value > 0, sprintf "%A is invalid" w.messagingSvcInfo.messagingServicePort

                    w.contGenInfo.partitionerId.value.value <> Guid.Empty, sprintf "%A is invalid" w.contGenInfo.partitionerId
                    w.contGenInfo.lastAllowedNodeErr.value > 0<minute>, sprintf "%A is invalid" w.contGenInfo.lastAllowedNodeErr
                ]
                |> List.fold(fun acc r -> combine acc r) (true, EmptyString)

            match r with
            | true, _ -> Ok()
            | false, s -> s |> InvalidSettings |> ContGenSettingsErr |> ContGenServiceErr |> Error
