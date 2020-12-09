namespace MessagingService

open Argu

open ClmSys.ClmWorker
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
                | MsgSaveSettings -> "saves settings to the config file."


    type MsgSvcArgs = WorkerArguments<MessagingServiceRunArgs>

    and
        [<CliPrefix(CliPrefix.None)>]
        MsgSvcArguArgs =
        | [<Unique>] [<First>] [<AltCommandLine("r")>] Run of ParseResults<MessagingServiceRunArgs>
        | [<Unique>] [<First>] [<AltCommandLine("s")>] Save of ParseResults<MessagingServiceRunArgs>

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Run _ -> "run messaging service from command line without installing."
                | Save _ -> "save parameters into the config file."


    let convertArgs s =
        match s with
        | Run a -> MsgSvcArgs.Run a
        | Save a -> MsgSvcArgs.Save a


    let tryGetMsgServiceAddress p = p |> List.tryPick (fun e -> match e with | MsgSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetMsgServicePort p = p |> List.tryPick (fun e -> match e with | MsgSvcPort p -> p |> ServicePort |> Some | _ -> None)
    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | MsgSaveSettings -> Some () | _ -> None)


    let loadSettings p =
        let w = loadMsgServiceSettings()
        let h = w.messagingSvcInfo.messagingServiceAccessInfo.httpServiceInfo
        let n = w.messagingSvcInfo.messagingServiceAccessInfo.netTcpServiceInfo

        let serviceAddress = tryGetMsgServiceAddress p |> Option.defaultValue h.httpServiceAddress
        let netTcpServicePort = tryGetMsgServicePort p |> Option.defaultValue n.netTcpServicePort
        let httpServiceInfo = HttpServiceAccessInfo.create serviceAddress h.httpServicePort h.httpServiceName
        let netTcpServiceInfo = NetTcpServiceAccessInfo.create serviceAddress netTcpServicePort n.netTcpServiceName
        let msgServiceAccessInfo = ServiceAccessInfo.create httpServiceInfo netTcpServiceInfo
        let messagingSvcInfo = MessagingServiceAccessInfo.create messagingDataVersion msgServiceAccessInfo

        let w1 = {w with messagingSvcInfo = messagingSvcInfo}

        w1


    let getServiceSettingsImpl b p =
        let load() = loadSettings p
        let tryGetSave() = tryGetSaveSettings p
        getMsgServiceInfo (load, tryGetSave) b


    let getServiceSettings = getServiceSettingsImpl false
    let saveSettings p = getServiceSettingsImpl true p |> ignore
