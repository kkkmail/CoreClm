namespace WorkerNodeAdm

open Argu
open System

open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.Primitives

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
        | [<Unique>] [<AltCommandLine("-f")>] WrkAdmForce of bool


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
                | WrkAdmForce _ -> "if true then forces to accept parameters, which otherwise would've been corrected by the system."


    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | WrkAdmSaveSettings -> Some () | _ -> None)


    let private proxy =
        {
            tryGetClientId = fun p -> p |> List.tryPick (fun e -> match e with | WrkAdmMsgCliId p -> p |> MessagingClientId |> WorkerNodeId |> Some | _ -> None)
            tryGetNodeName = fun p -> p |> List.tryPick (fun e -> match e with | WrkAdmName p -> p |> WorkerNodeName |> Some | _ -> None)
            tryGetPartitioner = fun p -> p |> List.tryPick (fun e -> match e with | WrkAdmPartitioner p -> p |> MessagingClientId |> PartitionerId |> Some | _ -> None)
            tryGetNoOfCores = fun p -> p |> List.tryPick (fun e -> match e with | WrkAdmNoOfCores p -> Some p | _ -> None)
            tryGetInactive = fun p -> p |> List.tryPick (fun e -> match e with | WrkAdmInactive p -> Some p | _ -> None)
            tryGetServiceAddress = fun p -> p |> List.tryPick (fun e -> match e with | WrkAdmSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
            tryGetServicePort = fun p -> p |> List.tryPick (fun e -> match e with | WrkAdmSvcPort p -> p |> ServicePort |> Some | _ -> None)
            tryGetMsgServiceAddress = fun p -> p |> List.tryPick (fun e -> match e with | WrkAdmMsgSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
            tryGetMsgServicePort = fun p -> p |> List.tryPick (fun e -> match e with | WrkAdmMsgSvcPort p -> p |> ServicePort |> Some | _ -> None)
            tryGetForce = fun p -> p |> List.tryPick (fun e -> match e with | WrkAdmForce p -> Some p | _ -> None)
        }


    let loadSettings p =
        match tryLoadSettings proxy p with
        | Some w ->
            printfn $"loadSettings: w = %A{w}"
            w
        | None -> invalidOp "Unable to load settings."


    let getServiceAccessInfoImpl b p =
        let load() = loadSettings p
        let trySave() = tryGetSaveSettings p
        getWorkerNodeServiceAccessInfo (load, trySave) b


    let getServiceAccessInfo p = getServiceAccessInfoImpl false p
    let saveSettings p = getServiceAccessInfoImpl true p |> ignore
