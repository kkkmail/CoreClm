namespace ClmSys

open System
open System.ServiceModel
open Softellect.Sys.Core

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
        {
            contGenServiceAddress : ContGenServiceAddress
            contGenServicePort : ContGenServicePort
            contGenServiceName : ContGenServiceName
        }

        member private s.serviceName = s.contGenServiceName.value.value
        member s.wcfServiceName = toValidServiceName s.serviceName
        member s.wcfServiceUrl = getWcfServiceUrlImpl s.contGenServiceAddress.value s.contGenServicePort.value s.wcfServiceName


    type ContGenWcfSvcShutDownInfo =
        {
//            contGenServiceHost : ServiceHost
            contGenServiceHost : int
        }


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
            messagingSvcInfo : MessagingServiceAccessInfo
        }

        member w.isValid() =
            let r =
                [
                    w.contGenSvcInfo.contGenServiceAddress.value.value <> EmptyString, sprintf "%A is invalid" w.contGenSvcInfo.contGenServiceAddress
                    w.contGenSvcInfo.contGenServicePort.value.value > 0, sprintf "%A is invalid" w.contGenSvcInfo.contGenServicePort

                    w.messagingSvcInfo.messagingServiceAddress.value.value <> EmptyString, sprintf "%A is invalid" w.messagingSvcInfo.messagingServiceAddress
                    w.messagingSvcInfo.messagingServicePort.value.value > 0, sprintf "%A is invalid" w.messagingSvcInfo.messagingServicePort

                    w.contGenInfo.partitionerId.value.value <> Guid.Empty, sprintf "%A is invalid" w.contGenInfo.partitionerId
                    w.contGenInfo.lastAllowedNodeErr.value > 0<minute>, sprintf "%A is invalid" w.contGenInfo.lastAllowedNodeErr
                ]
                |> List.fold(fun acc r -> combine acc r) (true, EmptyString)

            match r with
            | true, _ -> Ok()
            | false, s -> s |> InvalidSettings |> ContGenSettingsErr |> ContGenServiceErr |> Error

