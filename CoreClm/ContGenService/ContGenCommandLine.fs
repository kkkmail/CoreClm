namespace ContGenService

open System
open Argu

open Softellect.Sys.Primitives
open Softellect.Messaging.Primitives
open Softellect.Messaging.ServiceInfo
open Softellect.Wcf.Common
open Softellect.Messaging.Primitives
open Softellect.Messaging.Client
open Softellect.Sys.Worker
open Softellect.Messaging.Primitives
open Softellect.Messaging.Proxy
open Softellect.Messaging.ServiceProxy

open ClmSys.ClmWorker
open Primitives.VersionInfo
open ClmSys.Logging
open ClmSys.ClmErrors
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
open ClmSys.ContGenPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.ContGenData
open DbData.Configuration
open ContGenServiceInfo.ServiceInfo
open ServiceProxy.ModelRunnerProxy

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
                | SaveSettings -> "saves settings into config file."
                | Partitioner _ -> "messaging client id of a partitioner service."
                | MsgSvcAddress _ -> "messaging server ip address / name."
                | MsgSvcPort _ -> "messaging server port."

    and
        [<CliPrefix(CliPrefix.None)>]
        ContGenSvcArguArgs =
        | [<Unique>] [<First>] [<AltCommandLine("r")>] Run of ParseResults<ContGenRunArgs>
        | [<Unique>] [<First>] [<AltCommandLine("s")>] Save of ParseResults<ContGenRunArgs>

        with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Run _ -> "run ContGen service from command line without installing."
                | Save _ -> "save parameters into config file."


    type ContGenSvcArgs = WorkerArguments<ContGenRunArgs>


    let convertArgs s =
        match s with
        | Run a -> ContGenSvcArgs.Run a
        | Save a -> ContGenSvcArgs.Save a


    let tryGetServiceAddress p = p |> List.tryPick (fun e -> match e with | SvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetServicePort p = p |> List.tryPick (fun e -> match e with | SvcPort p -> p |> ServicePort |> Some | _ -> None)

    let tryGeMinUsefulEe p = p |> List.tryPick (fun e -> match e with | MinimumUsefulEe p -> p |> MinUsefulEe |> Some | _ -> None)
    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | SaveSettings -> Some () | _ -> None)
    let tryGetPartitioner p = p |> List.tryPick (fun e -> match e with | Partitioner p -> p |> MessagingClientId |> PartitionerId |> Some | _ -> None)

    let tryGetMsgServiceAddress p = p |> List.tryPick (fun e -> match e with | MsgSvcAddress s -> s |> ServiceAddress |> Some | _ -> None)
    let tryGetMsgServicePort p = p |> List.tryPick (fun e -> match e with | MsgSvcPort p -> p |> ServicePort |> Some | _ -> None)


    let loadContGenInfo (c : ContGenInfo) p =
        let contGenInfo =
            {
                c with
                    controlData = { c.controlData with minUsefulEe = tryGeMinUsefulEe p |> Option.defaultValue c.controlData.minUsefulEe }
                    partitionerId = tryGetPartitioner p |> Option.defaultValue c.partitionerId
            }

        contGenInfo


    let loadContGenServiceAccessInfo (c : ContGenServiceAccessInfo) p =
        let h = c.value.httpServiceInfo
        let n = c.value.netTcpServiceInfo

        let serviceAddress = tryGetServiceAddress p |> Option.defaultValue h.httpServiceAddress
        let netTcpServicePort = tryGetServicePort p |> Option.defaultValue n.netTcpServicePort
        let contGenSvcInfo = ContGenServiceAccessInfo.create serviceAddress h.httpServicePort netTcpServicePort WcfSecurityMode.defaultValue

        contGenSvcInfo


    let loadMessagingServiceAccessInfo (m : MessagingServiceAccessInfo) p =
        let h = m.messagingServiceAccessInfo.httpServiceInfo
        let n = m.messagingServiceAccessInfo.netTcpServiceInfo

        let serviceAddress = tryGetMsgServiceAddress p |> Option.defaultValue h.httpServiceAddress
        let netTcpServicePort = tryGetMsgServicePort p |> Option.defaultValue n.netTcpServicePort
        let httpServiceInfo = HttpServiceAccessInfo.create serviceAddress h.httpServicePort h.httpServiceName
        let netTcpServiceInfo = NetTcpServiceAccessInfo.create serviceAddress netTcpServicePort n.netTcpServiceName WcfSecurityMode.defaultValue
        let msgServiceAccessInfo = ServiceAccessInfo.create httpServiceInfo netTcpServiceInfo
        let messagingSvcInfo = MessagingServiceAccessInfo.create messagingDataVersion msgServiceAccessInfo

        messagingSvcInfo


    let loadSettings p =
        let w = loadContGenSettings()

        let w1 =
            {
                w with
                    contGenInfo = loadContGenInfo w.contGenInfo p
                    contGenSvcInfo = loadContGenServiceAccessInfo w.contGenSvcInfo p
                    messagingSvcInfo = loadMessagingServiceAccessInfo w.messagingSvcInfo p
            }

        w1


    let saveSettings p =
        let load() = loadSettings p
        let tryGet() = tryGetSaveSettings p
        saveContGenSettings load tryGet


    let getMessageProcessorProxy (d : MessagingClientAccessInfo) =
        let i =
            {
                messagingClientName = ContGenServiceName.netTcpServiceName.value.value |> MessagingClientName
                storageType = getContGenConnectionString |> MsSqlDatabase
                messagingDataVersion = messagingDataVersion
            }

        let getMessageSize (m : ClmMessageData) = m.getMessageSize()

        let messagingClientData =
            {
                msgAccessInfo = d
                communicationType = NetTcpCommunication
                msgClientProxy = createMessagingClientProxy getMessageSize i d.msgClientId
                expirationTime = MessagingClientData.defaultExpirationTime
                logOnError = true
            }

        printfn "tryGetContGenServiceData::Calling MessagingClient messagingClientData..."
        let messagingClient = MessagingClient messagingClientData
        messagingClient.messageProcessorProxy


    let tryGetContGenServiceData (logger : Logger) p : Result<ContGenServiceData, ClmError> =
        let w = loadSettings p
        printfn $"getContGenServiceData: w = %A{w}"

        let i =
            {
                msgClientId = w.contGenInfo.partitionerId.messagingClientId
                msgSvcAccessInfo = w.messagingSvcInfo
            }

        let data =
            {
                modelRunnerData =
                    {
                        runnerData =
                            {
                                getConnectionString = getContGenConnectionString
                                contGenInfo = w.contGenInfo
                            }

                        runnerProxy =
                            {
                                getMessageProcessorProxy = getMessageProcessorProxy
                            }

                        messagingClientAccessInfo = i
                        logger = logger
                    }

                contGenServiceAccessInfo = w.contGenSvcInfo
            }

        match w.isValid() with
        | Ok() -> Ok data
        | Error e -> Error e
