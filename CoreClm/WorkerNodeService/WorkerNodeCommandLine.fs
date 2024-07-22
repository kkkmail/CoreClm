namespace WorkerNodeService

open System
open Argu

open Softellect.Sys.Primitives
open Softellect.Messaging.Primitives

open ClmSys.PartitionerPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.ClmWorker
open WorkerNodeServiceInfo.ServiceInfo
open Softellect.Sys.Worker

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
        | [<Unique>] [<AltCommandLine("-f")>] WrkForce of bool

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
                | WrkForce _ -> "if true then forces to accept parameters, which otherwise would've been corrected by the system."


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
                | Run _ -> "run worker node service from command line."
                | Save _ -> "save parameters into config file."


    let convertArgs s =
        match s with
        | Run a -> WorkerNodeServiceArgs.Run a
        | Save a -> WorkerNodeServiceArgs.Save a


    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | WrkSaveSettings -> Some () | _ -> None)


    let private proxy =
        {
            tryGetClientId = fun p -> p |> List.tryPick (fun e -> match e with | WrkMsgCliId p -> p |> MessagingClientId |> WorkerNodeId |> Some | _ -> None)
            tryGetNodeName = fun p -> p |> List.tryPick (fun e -> match e with | WrkName p -> p |> WorkerNodeName |> Some | _ -> None)
            tryGetPartitioner = fun p -> p |> List.tryPick (fun e -> match e with | WrkPartitioner p -> p |> MessagingClientId |> PartitionerId |> Some | _ -> None)
            tryGetNoOfCores = fun p -> p |> List.tryPick (fun e -> match e with | WrkNoOfCores p -> Some p | _ -> None)
            tryGetInactive = fun p -> p |> List.tryPick (fun e -> match e with | WrkInactive p -> Some p | _ -> None)
            tryGetServiceAddress = fun p -> p |> List.tryPick (fun e -> match e with | WrkSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
            tryGetServicePort = fun p -> p |> List.tryPick (fun e -> match e with | WrkSvcPort p -> p |> ServicePort |> Some | _ -> None)
            tryGetMsgServiceAddress = fun p -> p |> List.tryPick (fun e -> match e with | WrkMsgSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
            tryGetMsgServicePort = fun p -> p |> List.tryPick (fun e -> match e with | WrkMsgSvcPort p -> p |> ServicePort |> Some | _ -> None)
            tryGetForce = fun p -> p |> List.tryPick (fun e -> match e with | WrkForce p -> Some p | _ -> None)
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
