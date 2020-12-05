namespace MessagingAdm

open Argu
open ClmSys.MessagingData
open ClmSys.GeneralPrimitives
open ClmSys.MessagingPrimitives
open MessagingServiceInfo.ServiceInfo

module AdmCommandLine =

    [<Literal>]
    let MsgAdmAppName = "MessagingAdm.exe"

    [<CliPrefix(CliPrefix.None)>]
    type MsgAdmRunArgs =
        | [<Unique>] [<First>] [<AltCommandLine("m")>] MonitorMsgService
        | [<Unique>] [<First>] [<AltCommandLine("start")>] StartMsgService
        | [<Unique>] [<First>] [<AltCommandLine("stop")>] StopMsgService
        | [<Unique>] [<AltCommandLine("-address")>] MsgSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] MsgSvcPort of int
        | [<Unique>] [<AltCommandLine("-save")>] MsgSaveSettings

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | MonitorMsgService _ -> "monitors messaging service."
                | StartMsgService _ -> "start messaging service."
                | StopMsgService _ -> "stop messaging service."
                | MsgSvcAddress _ -> "messaging server ip address / name."
                | MsgSvcPort _ -> "messaging server port."
                | MsgSaveSettings -> "saves settings to the Registry."


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
