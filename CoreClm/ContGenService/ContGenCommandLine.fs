namespace ContGenService

open Argu
open ClmSys.GeneralData
open ClmSys.ServiceInstaller
open System
open ClmSys.VersionInfo
open ClmSys.Logging
open Messaging.ServiceResponse
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
open Messaging.Client
open ClmSys.GeneralPrimitives
open ClmSys.ContGenPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.PartitionerPrimitives
open ContGen.ModelRunner
open ClmSys.MessagingData
open ClmSys.ContGenData
open Clm.ModelParams
open DbData.Configuration
open ContGenServiceInfo.ServiceInfo

module SvcCommandLine =

    type ContGenServiceData =
        {
            modelRunnerData : ModelRunnerDataWithProxy
            contGenServiceAccessInfo : ContGenServiceAccessInfo
        }


    [<CliPrefix(CliPrefix.Dash)>]
    type ContGenRunArgs =
        | [<Unique>] [<AltCommandLine("-address")>] SvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] SvcPort of int
        | [<Unique>] [<AltCommandLine("-ee")>] MinimumUsefulEe of double
        | [<Unique>] [<AltCommandLine("-save")>] SaveSettings
        | [<Unique>] [<AltCommandLine("-p")>] Partitioner of Guid
        | [<Unique>] [<AltCommandLine("-msgAddress")>] MsgSvcAddress of string
        | [<Unique>] [<AltCommandLine("-msgPort")>] MsgSvcPort of int

        with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | SvcAddress _ -> "cont gen service ip address / name."
                | SvcPort _ -> "cont gen service port."
                | MinimumUsefulEe _ -> "minimum useful ee to generate charts. Set to 0.0 to generate all charts."
                | SaveSettings -> "saves settings to the Registry."
                | Partitioner _ -> "messaging client id of a partitioner service."
                | MsgSvcAddress _ -> "messaging server ip address / name."
                | MsgSvcPort _ -> "messaging server port."

    and
        [<CliPrefix(CliPrefix.None)>]
        ContGenSvcArguArgs =
        | [<Unique>] [<First>] [<AltCommandLine("i")>] Install
        | [<Unique>] [<First>] [<AltCommandLine("u")>] Uninstall
        | [<Unique>] [<First>] Start
        | [<Unique>] [<First>] Stop
        | [<Unique>] [<First>] [<AltCommandLine("r")>] Run of ParseResults<ContGenRunArgs>
        | [<Unique>] [<First>] [<AltCommandLine("s")>] Save of ParseResults<ContGenRunArgs>

        with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Install -> "install ContGen service."
                | Uninstall -> "uninstall ContGen service."
                | Start -> "start ContGen service."
                | Stop -> "stop ContGen service."
                | Run _ -> "run ContGen service from command line without installing."
                | Save _ -> "save parameters into the registry."


    type ContGenSvcArgs = SvcArguments<ContGenRunArgs>


    let convertArgs s =
        match s with
        | Install -> ContGenSvcArgs.Install
        | Uninstall -> ContGenSvcArgs.Uninstall
        | Start -> ContGenSvcArgs.Start
        | Stop -> ContGenSvcArgs.Stop
        | Run a -> ContGenSvcArgs.Run a
        | Save a -> ContGenSvcArgs.Save a


    let tryGetServerAddress p = p |> List.tryPick (fun e -> match e with | SvcAddress s -> s |> ServiceAddress |> ContGenServiceAddress |> Some | _ -> None)
    let tryGetServerPort p = p |> List.tryPick (fun e -> match e with | SvcPort p -> p |> ServicePort |> ContGenServicePort |> Some | _ -> None)

    let tryGeMinUsefulEe p = p |> List.tryPick (fun e -> match e with | MinimumUsefulEe p -> p |> MinUsefulEe |> Some | _ -> None)
    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | SaveSettings -> Some () | _ -> None)
    let tryGetPartitioner p = p |> List.tryPick (fun e -> match e with | Partitioner p -> p |> MessagingClientId |> PartitionerId |> Some | _ -> None)

    let tryGetMsgServiceAddress p = p |> List.tryPick (fun e -> match e with | MsgSvcAddress s -> s |> ServiceAddress |> MessagingServiceAddress |> Some | _ -> None)
    let tryGetMsgServicePort p = p |> List.tryPick (fun e -> match e with | MsgSvcPort p -> p |> ServicePort |> MessagingServicePort |> Some | _ -> None)


    let loadSettings p =
        let w = loadContGenSettings()

        let w1 =
            {

                contGenInfo =
                    {
                        minUsefulEe = tryGeMinUsefulEe p |> Option.defaultValue w.contGenInfo.minUsefulEe
                        partitionerId = tryGetPartitioner p |> Option.defaultValue w.contGenInfo.partitionerId
                        lastAllowedNodeErr = w.contGenInfo.lastAllowedNodeErr
                        earlyExitCheckFreq = w.contGenInfo.earlyExitCheckFreq
                    }

                contGenSvcInfo =
                    {
                        contGenServiceAddress = tryGetServerAddress p |> Option.defaultValue w.contGenSvcInfo.contGenServiceAddress
                        contGenServicePort = tryGetServerPort p |> Option.defaultValue w.contGenSvcInfo.contGenServicePort
                        contGenServiceName = w.contGenSvcInfo.contGenServiceName
                    }

                messagingSvcInfo =
                    {
                        messagingServiceAddress = tryGetMsgServiceAddress p |> Option.defaultValue w.messagingSvcInfo.messagingServiceAddress
                        messagingServicePort = tryGetMsgServicePort p |> Option.defaultValue w.messagingSvcInfo.messagingServicePort
                        messagingServiceName = w.messagingSvcInfo.messagingServiceName
                    }
            }

        w1


    let saveSettings p =
        let load() = loadSettings p
        let tryGet() = tryGetSaveSettings p
        saveContGenSettings load tryGet


    /// TODO kk:20200517 - Propagate early exit info to command line parameters.
    let getContGenServiceData (logger : Logger) (p : list<ContGenRunArgs>) =
        let w = loadSettings p
        printfn "getContGenServiceData: w = %A" w

        let i =
            {
                msgClientId = w.contGenInfo.partitionerId.messagingClientId
                msgSvcAccessInfo = w.messagingSvcInfo
            }

        let getMessageProcessorProxy (d : MessagingClientAccessInfo) =
            let i =
                {
                    messagingClientName = contGenServiceName.value.messagingClientName
                    storageType = getClmConnectionString |> MsSqlDatabase
                }

            let messagingClientData =
                {
                    msgAccessInfo = d
                    messagingService = MsgResponseHandler d
                    msgClientProxy = MessagingClientProxy.create i d.msgClientId
                    expirationTime = MessagingClientData.defaultExpirationTime
                }

            let messagingClient = MessagingClient messagingClientData
            messagingClient.messageProcessorProxy

        let data =
            {
                modelRunnerData =
                    {
                        runnerData =
                            {
                                getConnectionString = getClmConnectionString
                                minUsefulEe = MinUsefulEe.defaultValue
                                resultLocation = DefaultResultLocationFolder

                                earlyExitInfoOpt =
                                    Some { EarlyExitInfo.defaultValue with
                                            frequency = TimeSpan.FromMinutes(w.contGenInfo.earlyExitCheckFreq.value / 1<minute> |> float) |> EarlyExitCheckFrequency}

                                lastAllowedNodeErr = w.contGenInfo.lastAllowedNodeErr
                            }

                        runnerProxy =
                            {
                                getMessageProcessorProxy = getMessageProcessorProxy
                                createMessagingEventHandlers = createMessagingClientEventHandlers
                            }

                        messagingClientAccessInfo = i
                        logger = logger
                    }

                contGenServiceAccessInfo = w.contGenSvcInfo
            }

        match w.isValid() with
        | Ok() -> Ok data
        | Error e -> Error e
