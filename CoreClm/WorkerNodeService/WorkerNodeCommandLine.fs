namespace WorkerNodeService

open System
open Argu

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

open ClmSys.VersionInfo
open ClmSys.WorkerNodeData
open ClmSys.GeneralPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.ClmWorker
open WorkerNodeServiceInfo.ServiceInfo

module SvcCommandLine =

    [<CliPrefix(CliPrefix.Dash)>]
    type WorkerNodeServiceRunArgs =
        | [<Unique>] [<AltCommandLine("-address")>] WrkSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] WrkSvcPort of int
        | [<Unique>] [<AltCommandLine("-n")>] WrkName of string
        | [<Unique>] [<AltCommandLine("-c")>] WrkNoOfCores of int

        | [<Unique>] [<AltCommandLine("-save")>] WrkSaveSettings

        | [<Unique>] [<AltCommandLine("-msgAddress")>] WrkMsgSvcAddress of string
        | [<Unique>] [<AltCommandLine("-msgPort")>] WrkMsgSvcPort of int

        | [<Unique>] [<AltCommandLine("-id")>] WrkMsgCliId of Guid
        | [<Unique>] [<AltCommandLine("-p")>] WrkPartitioner of Guid
        | [<Unique>] [<AltCommandLine("-i")>] WrkInactive of bool

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | WrkSvcAddress _ -> "worker node service ip address / name."
                | WrkSvcPort _ -> "worker node service port."
                | WrkName _ -> "worker node name."
                | WrkNoOfCores _ -> "number of processor cores used by current node. If nothing specified, then half of available logical cores are used."

                | WrkSaveSettings -> "saves settings into config file."

                | WrkMsgSvcAddress _ -> "messaging server ip address / name."
                | WrkMsgSvcPort _ -> "messaging server port."

                | WrkMsgCliId _ -> "messaging client id of current worker node service."
                | WrkPartitioner _ -> "messaging client id of a partitioner service."
                | WrkInactive _ -> "if true then worker node is inactive and it will unregister itself from the cluster."


    type WorkerNodeServiceArgs = WorkerArguments<WorkerNodeServiceRunArgs>

    and
        [<CliPrefix(CliPrefix.None)>]
        WorkerNodeServiceArguArgs =
        | [<Unique>] [<First>] [<AltCommandLine("r")>] Run of ParseResults<WorkerNodeServiceRunArgs>
        | [<Unique>] [<First>] [<AltCommandLine("s")>] Save of ParseResults<WorkerNodeServiceRunArgs>

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Run _ -> "run worker node service from command line without installing."
                | Save _ -> "save parameters into config file."


    let convertArgs s =
        match s with
        | Run a -> WorkerNodeServiceArgs.Run a
        | Save a -> WorkerNodeServiceArgs.Save a


    let tryGetServiceAddress p = p |> List.tryPick (fun e -> match e with | WrkSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetServicePort p = p |> List.tryPick (fun e -> match e with | WrkSvcPort p -> p |> ServicePort |> Some | _ -> None)
    let tryGetNodeName p = p |> List.tryPick (fun e -> match e with | WrkName p -> p |> WorkerNodeName |> Some | _ -> None)
    let tryGetNoOfCores p = p |> List.tryPick (fun e -> match e with | WrkNoOfCores p -> Some p | _ -> None)
    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | WrkSaveSettings -> Some () | _ -> None)
    let tryGetMsgServiceAddress p = p |> List.tryPick (fun e -> match e with | WrkMsgSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetMsgServicePort p = p |> List.tryPick (fun e -> match e with | WrkMsgSvcPort p -> p |> ServicePort |> Some | _ -> None)
    let tryGetPartitioner p = p |> List.tryPick (fun e -> match e with | WrkPartitioner p -> p |> MessagingClientId |> PartitionerId |> Some | _ -> None)
    let tryGetClientId p = p |> List.tryPick (fun e -> match e with | WrkMsgCliId p -> p |> MessagingClientId |> WorkerNodeId |> Some | _ -> None)
    let tryGetInactive p = p |> List.tryPick (fun e -> match e with | WrkInactive p -> Some p | _ -> None)


    let loadSettings p =
        let workerNodeId = tryGetClientId p
        let workerNodeName = tryGetNodeName p

        match tryLoadWorkerNodeSettings workerNodeId workerNodeName with
        | Some w ->
            let wn = w.workerNodeSvcInfo.value.netTcpServiceInfo
            let mn = w.messagingSvcInfo.messagingServiceAccessInfo.netTcpServiceInfo

            let w1 =
                {
                    workerNodeInfo =
                        { w.workerNodeInfo with
                            partitionerId = tryGetPartitioner p |> Option.defaultValue w.workerNodeInfo.partitionerId

                            noOfCores =
                                let n = tryGetNoOfCores p |> Option.defaultValue w.workerNodeInfo.noOfCores
                                max 0 (min n Environment.ProcessorCount)

                            nodePriority =
                                match w.workerNodeInfo.nodePriority.value with
                                | x when x <= 0 -> WorkerNodePriority.defaultValue
                                | _ -> w.workerNodeInfo.nodePriority

                            isInactive = tryGetInactive p |> Option.defaultValue w.workerNodeInfo.isInactive
                            lastErrorDateOpt = w.workerNodeInfo.lastErrorDateOpt
                        }

                    workerNodeSvcInfo =
                        { w.workerNodeSvcInfo.value with
                            netTcpServiceInfo =
                                { wn with
                                    netTcpServiceAddress = tryGetServiceAddress p |> Option.defaultValue wn.netTcpServiceAddress
                                    netTcpServicePort = tryGetServicePort p |> Option.defaultValue wn.netTcpServicePort
                                }
                        }
                        |> WorkerNodeServiceAccessInfo

                    workerNodeCommunicationType = w.workerNodeCommunicationType

                    messagingSvcInfo =
                        { w.messagingSvcInfo with
                            messagingServiceAccessInfo =
                                { w.messagingSvcInfo.messagingServiceAccessInfo with
                                    netTcpServiceInfo =
                                        { mn with
                                            netTcpServiceAddress = tryGetMsgServiceAddress p |> Option.defaultValue mn.netTcpServiceAddress
                                            netTcpServicePort = tryGetMsgServicePort p |> Option.defaultValue mn.netTcpServicePort
                                        }
                                }
                            messagingDataVersion = messagingDataVersion
                        }

                    messagingCommunicationType = w.messagingCommunicationType
                }

            printfn "loadSettings: w1 = %A" w1
            w1
        | None -> invalidOp "Unable to load settings."

    let getServiceAccessInfoImpl b p =
        let load() = loadSettings p
        let trySave() = tryGetSaveSettings p
        getWorkerNodeServiceAccessInfo (load, trySave) b


    let getServiceAccessInfo p = getServiceAccessInfoImpl false p
    let saveSettings p = getServiceAccessInfoImpl true p |> ignore
