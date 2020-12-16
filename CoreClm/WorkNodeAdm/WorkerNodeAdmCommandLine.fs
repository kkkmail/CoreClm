﻿namespace WorkerNodeAdm

open Argu
open System

open Softellect.Sys.Logging
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.ServiceInstaller
open Softellect.Sys.Primitives
open Softellect.Sys.Core
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.MessagingServiceErrors
open Softellect.Messaging.ServiceInfo
open Softellect.Sys.Core
open Softellect.Sys.Primitives
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.Logging
open Softellect.Sys.MessagingErrors
open Softellect.Wcf.Common
open Softellect.Wcf.Service
open Softellect.Messaging.Primitives
open Softellect.Messaging.ServiceInfo
open Softellect.Messaging.Service
open Softellect.Messaging.Client
open Softellect.Messaging.Proxy
open Softellect.Sys.MessagingClientErrors
open Softellect.Sys.MessagingServiceErrors

open ClmSys.WorkerNodeData
open ClmSys.GeneralPrimitives
open ClmSys.GeneralData
open ClmSys.MessagingPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.PartitionerPrimitives
open WorkerNodeServiceInfo.ServiceInfo

module AdmCommandLine =

    let WrkAdmAppName = "WorkerNodeAdm.exe"


    [<CliPrefix(CliPrefix.Dash)>]
    type WorkerNodeAdmArgs =
        //| [<Unique>] [<First>] [<AltCommandLine("c")>] ConfigureWrkService
        | [<Unique>] [<First>] [<AltCommandLine("m")>] MonitorWrkService

        | [<Unique>] [<AltCommandLine("-address")>] WrkAdmSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] WrkAdmSvcPort of int
        | [<Unique>] [<AltCommandLine("-n")>] WrkAdmName of string
        | [<Unique>] [<AltCommandLine("-c")>] WrkAdmNoOfCores of int

        | [<Unique>] [<AltCommandLine("-save")>] WrkAdmSaveSettings

        | [<Unique>] [<AltCommandLine("-msgAddress")>] WrkAdmMsgSvcAddress of string
        | [<Unique>] [<AltCommandLine("-msgPort")>] WrkAdmMsgSvcPort of int

        | [<Unique>] [<AltCommandLine("-id")>] WrkAdmMsgCliId of Guid
        | [<Unique>] [<AltCommandLine("-p")>] WrkAdmPartitioner of Guid
        | [<Unique>] [<AltCommandLine("-i")>] WrkAdmInactive of bool

        with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                //| ConfigureWrkService -> "configures running worker node service."
                | MonitorWrkService -> "monitors running worker node service."

                | WrkAdmSvcAddress _ -> "worker node service ip address / name."
                | WrkAdmSvcPort _ -> "worker node service port."
                | WrkAdmName _ -> "worker node name."
                | WrkAdmNoOfCores _ -> "number of processor cores used by current node. If nothing specified, then half of available logical cores are used."

                | WrkAdmSaveSettings -> "saves settings into config file."

                | WrkAdmMsgSvcAddress _ -> "messaging server ip address / name."
                | WrkAdmMsgSvcPort _ -> "messaging server port."

                | WrkAdmMsgCliId _ -> "messaging client id of current worker node service."
                | WrkAdmPartitioner _ -> "messaging client id of a partitioner service."
                | WrkAdmInactive _ -> "if true then worker node is inactive and it will unregister itself from the cluster."


    let tryGetServiceAddress p = p |> List.tryPick (fun e -> match e with | WrkAdmSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetServicePort p = p |> List.tryPick (fun e -> match e with | WrkAdmSvcPort p -> p |> ServicePort |> Some | _ -> None)
    let tryGetNodeName p = p |> List.tryPick (fun e -> match e with | WrkAdmName p -> p |> WorkerNodeName |> Some | _ -> None)
    let tryGetNoOfCores p = p |> List.tryPick (fun e -> match e with | WrkAdmNoOfCores p -> Some p | _ -> None)

    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | WrkAdmSaveSettings -> Some () | _ -> None)

    let tryGetMsgServiceAddress p = p |> List.tryPick (fun e -> match e with | WrkAdmMsgSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetMsgServicePort p = p |> List.tryPick (fun e -> match e with | WrkAdmMsgSvcPort p -> p |> ServicePort |> Some | _ -> None)

    let tryGetPartitioner p = p |> List.tryPick (fun e -> match e with | WrkAdmPartitioner p -> p |> MessagingClientId |> PartitionerId |> Some | _ -> None)
    let tryGetClientId p = p |> List.tryPick (fun e -> match e with | WrkAdmMsgCliId p -> p |> MessagingClientId |> WorkerNodeId |> Some | _ -> None)
    let tryGetInactive p = p |> List.tryPick (fun e -> match e with | WrkAdmInactive p -> Some p | _ -> None)


    let loadSettings p =
        let workerNodeId = tryGetClientId p
        let workerNodeName = tryGetNodeName p

        let w = tryLoadWorkerNodeSettings workerNodeId workerNodeName
        failwith ""

//        let w1 =
//            {
//                workerNodeInfo =
//                    {
//                        workerNodeId = tryGetClientId p |> Option.defaultValue w.workerNodeInfo.workerNodeId
//                        workerNodeName = tryGetNodeName p |> Option.defaultValue w.workerNodeInfo.workerNodeName
//                        partitionerId = tryGetPartitioner p |> Option.defaultValue w.workerNodeInfo.partitionerId
//                        noOfCores = 0
//
//                        nodePriority =
//                            let n = tryGetNoOfCores p |> Option.defaultValue w.workerNodeInfo.noOfCores
//                            max 0 (min n Environment.ProcessorCount) |> WorkerNodePriority
//
//                        isInactive = tryGetInactive p |> Option.defaultValue w.workerNodeInfo.isInactive
//                        lastErrorDateOpt = w.workerNodeInfo.lastErrorDateOpt
//                    }
//
//                workerNodeSvcInfo =
//                    {
//                        workerNodeServiceAddress = tryGetServiceAddress p |> Option.defaultValue w.workerNodeSvcInfo.workerNodeServiceAddress
//                        workerNodeServicePort = tryGetServicePort p |> Option.defaultValue w.workerNodeSvcInfo.workerNodeServicePort
//                        workerNodeServiceName = w.workerNodeSvcInfo.workerNodeServiceName
//                    }
//
//                messagingSvcInfo =
//                    {
//                        messagingServiceAddress = tryGetMsgServiceAddress p |> Option.defaultValue w.messagingSvcInfo.messagingServiceAddress
//                        messagingServicePort = tryGetMsgServicePort p |> Option.defaultValue w.messagingSvcInfo.messagingServicePort
//                        messagingServiceName = w.messagingSvcInfo.messagingServiceName
//                    }
//            }
//
//        printfn "loadSettings: w1 = %A" w1
//        w1


    let getServiceAccessInfoImpl b p =
        let load() = loadSettings p
        let trySave() = tryGetSaveSettings p
        getWorkerNodeServiceAccessInfo (load, trySave) b


    let getServiceAccessInfo p = getServiceAccessInfoImpl false p
    let saveSettings p = getServiceAccessInfoImpl true p |> ignore
