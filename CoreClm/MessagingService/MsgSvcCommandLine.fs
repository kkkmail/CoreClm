namespace MessagingService

open Argu
open Softellect.Sys.ServiceInstaller
open ClmSys.MessagingData
open ClmSys.GeneralPrimitives
open ClmSys.MessagingPrimitives
open MessagingServiceInfo.ServiceInfo

module SvcCommandLine =

    [<CliPrefix(CliPrefix.Dash)>]
    type MessagingServiceRunArgs =
        | [<Unique>] [<AltCommandLine("-address")>] MsgSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] MsgSvcPort of int
        | [<Unique>] [<AltCommandLine("-save")>] MsgSaveSettings

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | MsgSvcAddress _ -> "messaging server ip address / name."
                | MsgSvcPort _ -> "messaging server port."
                | MsgSaveSettings -> "saves settings to the Registry."


    type MsgSvcArgs = SvcArguments<MessagingServiceRunArgs>

    and
        [<CliPrefix(CliPrefix.None)>]
        MsgSvcArguArgs =
        | [<Unique>] [<First>] [<AltCommandLine("i")>] Install
        | [<Unique>] [<First>] [<AltCommandLine("u")>] Uninstall
        | [<Unique>] [<First>] Start
        | [<Unique>] [<First>] Stop
        | [<Unique>] [<First>] [<AltCommandLine("r")>] Run of ParseResults<MessagingServiceRunArgs>
        | [<Unique>] [<First>] [<AltCommandLine("s")>] Save of ParseResults<MessagingServiceRunArgs>

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Install -> "install messaging service."
                | Uninstall -> "uninstall messaging service."
                | Start _ -> "start messaging service."
                | Stop -> "stop messaging service."
                | Run _ -> "run messaging service from command line without installing."
                | Save _ -> "save parameters into the registry."


    let convertArgs s =
        match s with
        | Install -> MsgSvcArgs.Install
        | Uninstall -> MsgSvcArgs.Uninstall
        | Start -> MsgSvcArgs.Start
        | Stop -> MsgSvcArgs.Stop
        | Run a -> MsgSvcArgs.Run a
        | Save a -> MsgSvcArgs.Save a


    let tryGetMsgServiceAddress p = p |> List.tryPick (fun e -> match e with | MsgSvcAddress s -> s |> ServiceAddress |> MessagingServiceAddress |> Some | _ -> None)
    let tryGetMsgServicePort p = p |> List.tryPick (fun e -> match e with | MsgSvcPort p -> p |> ServicePort |> MessagingServicePort |> Some | _ -> None)
    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | MsgSaveSettings -> Some () | _ -> None)


    let loadSettings p =
        let w = loadMsgServiceSettings()

        let w1 =
            {
                messagingInfo =
                    {
                        expirationTime = w.messagingInfo.expirationTime
                    }

                messagingSvcInfo =
                    {
                        messagingServiceAddress = tryGetMsgServiceAddress p |> Option.defaultValue w.messagingSvcInfo.messagingServiceAddress
                        messagingServicePort = tryGetMsgServicePort p |> Option.defaultValue w.messagingSvcInfo.messagingServicePort
                        messagingServiceName = w.messagingSvcInfo.messagingServiceName
                    }
            }

        w1


    let getServiceAccessInfoImpl b p =
        let load() = loadSettings p
        let tryGetSave() = tryGetSaveSettings p
        getMsgServiceInfo (load, tryGetSave) b


    let getServiceAccessInfo = getServiceAccessInfoImpl false
    let saveSettings p = getServiceAccessInfoImpl true p |> ignore
