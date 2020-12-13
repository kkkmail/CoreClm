namespace ContGenServiceInfo

open System
open System.ServiceModel
open System.Threading

open Softellect.Messaging
open Softellect.Sys
open Softellect.Sys.Core
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.AppSettings
open Softellect.Wcf.Common
open Softellect.Messaging.Primitives
open Softellect.Sys.Core
open Softellect.Sys.ServiceInstaller
open Softellect.Sys.Primitives
open Softellect.Sys.Core
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.MessagingServiceErrors
open Softellect.Messaging.ServiceInfo
open Softellect.Wcf.Common
open Softellect.Wcf.Service

open ClmSys.VersionInfo
open ClmSys.MessagingPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.GeneralData
open Clm.ModelParams
open ClmSys.GeneralPrimitives
open ClmSys.SolverRunnerPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.ContGenPrimitives
open ClmSys.ClmErrors
open ClmSys.ContGenErrors
open ClmSys.ContGenData
open ClmSys.PartitionerData

module ServiceInfo =

    let contGenServiceProgramName = "ContGenService.exe"


    [<Literal>]
    let ContGenWcfServiceName = "ContGenWcfService"


    type RunningProcessData =
        {
            modelDataId : ModelDataId
            defaultValueId : ClmDefaultValueId
            runQueueId : RunQueueId
            workerNodeId : WorkerNodeId
            commandLineParams : ModelCommandLineParam
        }


    type ProgressUpdateInfo =
        {
            runQueueId : RunQueueId
            progress : TaskProgress
        }


    type RunningProcessInfo =
        {
            started : DateTime
            progressUpdateInfo : ProgressUpdateInfo
        }


    type ProgressUpdateInfo
        with
        member this.toRunningProcessInfo() =
            {
                started = DateTime.Now
                progressUpdateInfo = this
            }


    let mutable private callCount = -1


    let getServiceState (getState : unit -> (list<RunQueue> * UnitResult)) =
        if Interlocked.Increment(&callCount) = 0
        then
            try
                printfn "Getting state at %s ..." (DateTime.Now.ToString("yyyy-MM-dd.HH:mm:ss"))
                let (q, e) = getState()
                let r0 = q |> List.sortBy (fun e -> e.progress) |> List.map (fun e -> "      " + e.ToString()) |> String.concat Nl
                let r = if r0 = EmptyString then "[]" else Nl + "    [" + Nl + r0 + Nl + "    ]"
                printfn "... state at %s\n{\n  running = %s\n  runningCount = %A\n }"  (DateTime.Now.ToString("yyyy-MM-dd.HH:mm:ss")) r q.Length
            with
            | e -> printfn "Exception occurred: %A" e
        else
            printfn "Not getting state at %A because callCount = %A." DateTime.Now callCount
            ignore()

        Interlocked.Decrement(&callCount) |> ignore
        Ok()


    type IContGenService =
        abstract tryCancelRunQueue : RunQueueId -> CancellationType -> UnitResult
        abstract tryRequestResults : RunQueueId -> ResultNotificationType -> UnitResult


    /// https://gist.github.com/dgfitch/661656
    [<ServiceContract(ConfigurationName = ContGenWcfServiceName)>]
    type IContGenWcfService =

        [<OperationContract(Name = "tryCancelRunQueue")>]
        abstract tryCancelRunQueue : q:byte[] -> byte[]

        [<OperationContract(Name = "tryRequestResults")>]
        abstract tryRequestResults : q:byte[] -> byte[]


    type ContGenServiceData =
        {
            modelRunnerData : ModelRunnerDataWithProxy
            contGenServiceAccessInfo : ContGenServiceAccessInfo
        }



    ////type MessagingClient = MessagingClient<ClmMessageData, ClmError>
    ////type MessagingClientData = MessagingClientData<ClmMessageData, ClmError>
    ////type MessagingServiceData = MessagingServiceData<ClmMessageData, ClmError>
    //type MessagingWcfServiceData = WcfServiceData<MessagingServiceData<ClmMessageData, ClmError>>
    //type Message = Message<ClmMessageData>
    //type MessageInfo = MessageInfo<ClmMessageData>
    //type MessagingService = MessagingService<ClmMessageData, ClmError>
    //type MessagingWcfService = MessagingWcfService<ClmMessageData, ClmError>
    //type MessagingWcfServiceImpl = WcfService<MessagingWcfService, IMessagingWcfService, MessagingServiceData>


    let contGenServiceAddress = ConfigKey "ContGenServiceAddress"
    let contGenServiceHttpPort = ConfigKey "ContGenServiceHttpPort"
    let contGenServiceNetTcpPort = ConfigKey "ContGenServiceNetTcpPort"
    let contGenServiceCommunicationType = ConfigKey "ContGenServiceCommunicationType"

    let messagingServiceAddress = ConfigKey "MessagingServiceAddress"
    let messagingHttpServicePort = ConfigKey "MessagingHttpServicePort"
    let messagingNetTcpServicePort = ConfigKey "MessagingNetTcpServicePort"
    let messagingServiceCommunicationType = ConfigKey "MessagingServiceCommunicationType"

    let minUsefulEe = ConfigKey "MinUsefulEe"
    let partitionerId = ConfigKey "PartitionerId"
    let lastAllowedNodeErrInMinutes = ConfigKey "LastAllowedNodeErrInMinutes"
    let earlyExitCheckFrequencyInMinutes = ConfigKey "EarlyExitCheckFrequencyInMinutes"


    let updateContGenSettings (provider : AppSettingsProvider) (c : ContGenServiceAccessInfo) (ct : WcfCommunicationType)  =
        let h = c.value.httpServiceInfo
        let n = c.value.netTcpServiceInfo

        provider.trySet contGenServiceAddress n.netTcpServiceAddress.value |> ignore
        provider.trySet contGenServiceHttpPort h.httpServicePort.value |> ignore
        provider.trySet contGenServiceNetTcpPort n.netTcpServicePort.value |> ignore
        provider.trySet contGenServiceCommunicationType ct.value |> ignore


    let updateMessagingSettings (provider : AppSettingsProvider) (m : MessagingServiceAccessInfo) (ct : WcfCommunicationType)  =
        let mh = m.messagingServiceAccessInfo.httpServiceInfo
        let mn = m.messagingServiceAccessInfo.netTcpServiceInfo

        provider.trySet messagingServiceAddress mn.netTcpServiceAddress.value |> ignore
        provider.trySet messagingHttpServicePort mh.httpServicePort.value |> ignore
        provider.trySet messagingNetTcpServicePort mn.netTcpServicePort.value |> ignore
        provider.trySet messagingServiceCommunicationType ct.value |> ignore


    type ContGenSettings
        with

        member w.trySaveSettings() =
            let toErr e = e |> ContGenSettingExn |> ContGenSettingsErr |> ContGenServiceErr |> Error

            match w.isValid(), AppSettingsProvider.tryCreate appSettingsFile with
            | Ok(), Ok provider ->
                try
                    updateContGenSettings provider w.contGenSvcInfo w.contGenCommType
                    updateMessagingSettings provider w.messagingSvcInfo w.messagingCommType

                    provider.trySet minUsefulEe w.contGenInfo.minUsefulEe.value |> ignore
                    provider.trySet partitionerId w.contGenInfo.partitionerId.value.value |> ignore
                    provider.trySet lastAllowedNodeErrInMinutes (w.contGenInfo.lastAllowedNodeErr.value / 1<minute>) |> ignore

                    provider.trySave() |> Rop.bindError toErr
                with
                | e -> toErr e
            | Error e, _ -> Error e
            | _, Error e -> toErr e


    type AppSettingsProviderResult = Result<AppSettingsProvider, exn>


    let getServiceAddress (providerRes : AppSettingsProviderResult) n d =
        match providerRes with
        | Ok provider ->
            match provider.tryGetString n with
            | Ok (Some EmptyString) -> d
            | Ok (Some s) -> s
            | _ -> d
        | _ -> d
        |> ServiceAddress


    let getServiceHttpPort (providerRes : AppSettingsProviderResult) n d =
        match providerRes with
        | Ok provider ->
            match provider.tryGetInt n with
            | Ok (Some k) when k > 0 -> k
            | _ -> d
        | _ -> d
        |> ServicePort


    let getServiceNetTcpPort (providerRes : AppSettingsProviderResult) n d =
        match providerRes with
        | Ok provider ->
            match provider.tryGetInt n with
            | Ok (Some k) when k > 0 -> k
            | _ -> d
        | _ -> d
        |> ServicePort


    let getCommunicationType (providerRes : AppSettingsProviderResult) n d =
        match providerRes with
        | Ok provider ->
            match provider.tryGetString n with
            | Ok (Some s) -> WcfCommunicationType.tryCreate s |> Option.defaultValue NetTcpCommunication
            | _ -> d
        | _ -> d


    let getPartitionerId (providerRes : AppSettingsProviderResult) n d =
        match providerRes with
        | Ok provider ->
            match provider.tryGetGuid n with
            | Ok (Some p) when p <> Guid.Empty -> p |> MessagingClientId |> PartitionerId
            | _ -> d
        | _ -> d


    let loadMessagingSettings providerRes =
        let messagingServiceCommunicationType = getCommunicationType providerRes messagingServiceCommunicationType NetTcpCommunication
        let serviceAddress = getServiceAddress providerRes messagingServiceAddress defaultMessagingServiceAddress
        let httpServicePort = getServiceHttpPort providerRes messagingHttpServicePort defaultMessagingHttpServicePort
        let netTcpServicePort = getServiceNetTcpPort providerRes messagingNetTcpServicePort defaultMessagingNetTcpServicePort

        let h = HttpServiceAccessInfo.create serviceAddress httpServicePort messagingHttpServiceName.value
        let n = NetTcpServiceAccessInfo.create serviceAddress netTcpServicePort messagingNetTcpServiceName.value
        let m = ServiceAccessInfo.create h n
        let messagingSvcInfo = MessagingServiceAccessInfo.create messagingDataVersion m

        messagingSvcInfo, messagingServiceCommunicationType


    let loadContGenServiceSettings providerRes =
        let contGenServiceAddress = getServiceAddress providerRes contGenServiceAddress defaultContGenServiceAddress
        let contGenServiceHttpPort = getServiceHttpPort providerRes contGenServiceHttpPort defaultContGenHttpServicePort
        let contGenServiceNetTcpPort = getServiceNetTcpPort providerRes contGenServiceNetTcpPort defaultContGenNetTcpServicePort
        let contGenServiceCommunicationType = getCommunicationType providerRes contGenServiceCommunicationType NetTcpCommunication

        let contGenSvcInfo = ContGenServiceAccessInfo.create contGenServiceAddress contGenServiceHttpPort contGenServiceNetTcpPort

        (contGenSvcInfo, contGenServiceCommunicationType)


    let loadContGenSettings() =
        let providerRes = AppSettingsProvider.tryCreate appSettingsFile

        let contGenInfo =
            match providerRes with
            | Ok provider ->
                {
                    minUsefulEe =
                        match provider.tryGetDecimal minUsefulEe with
                        | Ok (Some ee) -> ee |> double |> MinUsefulEe
                        | _ -> MinUsefulEe.defaultValue

                    partitionerId = getPartitionerId providerRes partitionerId defaultPartitionerId

                    lastAllowedNodeErr =
                        match provider.tryGetInt lastAllowedNodeErrInMinutes with
                        | Ok (Some p) when p > 0 -> p * 1<minute> |> LastAllowedNodeErr
                        | _ -> LastAllowedNodeErr.defaultValue

                    earlyExitCheckFreq =
                        match provider.tryGetInt earlyExitCheckFrequencyInMinutes with
                        | Ok (Some p) when p > 0 -> p * 1<minute> |> EarlyExitCheckFreq
                        | _ -> EarlyExitCheckFreq.defaultValue
                }
            | _ ->
                {
                    minUsefulEe = MinUsefulEe.defaultValue
                    partitionerId = defaultPartitionerId
                    lastAllowedNodeErr = LastAllowedNodeErr.defaultValue
                    earlyExitCheckFreq = EarlyExitCheckFreq.defaultValue
                }

        let (contGenSvcInfo, contGenServiceCommunicationType) = loadContGenServiceSettings providerRes
        let (messagingSvcInfo, messagingServiceCommunicationType) = loadMessagingSettings providerRes

        let w =
            {
                contGenInfo = contGenInfo
                contGenSvcInfo = contGenSvcInfo
                contGenCommType = contGenServiceCommunicationType
                messagingSvcInfo = messagingSvcInfo
                messagingCommType = messagingServiceCommunicationType
            }

        w


    let saveContGenSettings loadSettings tryGetSaveSettings =
        let (w : ContGenSettings) = loadSettings()

        let r =
            match tryGetSaveSettings() with
            | Some() -> w.trySaveSettings()
            | None -> Ok()

        match r with
        | Ok() -> printfn "Successfully saved settings."
        | Error e -> printfn "Error occurred trying to save settings: %A." e
