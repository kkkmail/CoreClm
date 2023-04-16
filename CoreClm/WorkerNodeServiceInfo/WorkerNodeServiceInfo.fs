namespace WorkerNodeServiceInfo

open System
open System.Threading

open ClmSys
open ClmSys.SolverData
open ClmSys.SolverRunnerPrimitives
open OdeSolver.Solver
open Softellect.Sys
open Softellect.Sys.Primitives
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.AppSettings
open Softellect.Wcf.Common
open Softellect.Wcf.Client
open Softellect.Messaging.ServiceInfo

open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.ClmErrors
open ClmSys.GeneralPrimitives
open ClmSys.WorkerNodeData
open System.ServiceModel
open ClmSys.WorkerNodeErrors
open ClmSys.PartitionerPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.PartitionerData
open ContGenServiceInfo.ServiceInfo

module ServiceInfo =

    let workerNodeServiceProgramName = "WorkerNodeService.exe"


    [<Literal>]
    let WorkerNodeWcfServiceName = "WorkerNodeWcfService"


    type WorkerNodeConfigParam =
        | WorkerNumberOfSores of int


    type RunnerState =
        {
            progressData : ClmProgressData
            started : DateTime
            lastUpdated : DateTime
        }

        static member defaultValue =
            {
                progressData = ClmProgressData.defaultValue
                started = DateTime.Now
                lastUpdated = DateTime.Now
            }

        override r.ToString() =
            let s = (DateTime.Now - r.started).ToString("d\.hh\:mm")

            let estCompl =
                match r.progressData.estimateEndTime r.started with
                | Some e -> " ETC: " + e.ToString("yyyy-MM-dd.HH:mm") + ";"
                | None -> EmptyString

            $"T: %s{s};%s{estCompl} %A{r.progressData.progressData.progress}"


    type WorkerNodeState =
        | NotStartedWorkerNode
        | StartedWorkerNode


    type WorkerNodeRunnerState =
        {
            workerNodeState : WorkerNodeState
        }

    type WorkerNodeMonitorParam =
        | DummyWrkMonitorParam


    type WorkerNodeMonitorResponse =
        | CannotAccessWrkNode
        | ErrorOccurred of ClmError

        override this.ToString() =
            match this with
            | CannotAccessWrkNode -> "Cannot access worker node."
            | ErrorOccurred e -> "Error occurred: " + e.ToString()


    type IWorkerNodeService =
        abstract monitor : WorkerNodeMonitorParam -> ClmResult<WorkerNodeMonitorResponse>

        /// To check if service is working.
        abstract ping : unit -> UnitResult


    let mutable private callCount = -1


    let getServiceState (service : IWorkerNodeService) p =
        if Interlocked.Increment(&callCount) = 0
        then
            try
                printfn "Getting worker node state at %s ..." (DateTime.Now.ToString("yyyy-MM-dd.HH:mm:ss"))
                let state = service.monitor p

                match state with
                | Ok r -> printfn "...state at %s =\n%s\n\n" (DateTime.Now.ToString("yyyy-MM-dd.HH:mm:ss")) (r.ToString())
                | Error e -> printfn "...state at %s =\n%A\n\n" (DateTime.Now.ToString("yyyy-MM-dd.HH:mm:ss")) e
            with
            | e -> printfn $"Exception occurred: %A{e}"
        else
            printfn $"Not getting state at %A{DateTime.Now} because callCount = %A{callCount}."

        Interlocked.Decrement(&callCount) |> ignore


    /// https://gist.github.com/dgfitch/661656
    [<ServiceContract(ConfigurationName = WorkerNodeWcfServiceName)>]
    type IWorkerNodeWcfService =

//        [<OperationContract(Name = "configure")>]
//        abstract configure : q:byte[] -> byte[]

        [<OperationContract(Name = "monitor")>]
        abstract monitor : q:byte[] -> byte[]

        [<OperationContract(Name = "ping")>]
        abstract ping : q:byte[] -> byte[]


    /// Low level WCF messaging client.
    type WorkerNodeResponseHandler private (url, communicationType, securityMode) =
        let tryGetWcfService() = tryGetWcfService<IWorkerNodeWcfService> communicationType securityMode url

        let configureWcfErr e = e |> ConfigureWcfErr |> WorkerNodeWcfErr |> WorkerNodeServiceErr
        let monitorWcfErr e = e |> MonitorWcfErr |> WorkerNodeWcfErr |> WorkerNodeServiceErr
        let pingWcfErr e = e |> PingWcfErr |> WorkerNodeWcfErr |> WorkerNodeServiceErr

        let monitorImpl p = tryCommunicate tryGetWcfService (fun service -> service.monitor) monitorWcfErr p
        let pingImpl() = tryCommunicate tryGetWcfService (fun service -> service.ping) pingWcfErr ()

        interface IWorkerNodeService with
            member _.monitor p = monitorImpl p
            member _.ping() = pingImpl()

        new (i : WorkerNodeServiceAccessInfo, communicationType, securityMode) =
            WorkerNodeResponseHandler(i.value.getUrl communicationType, communicationType, securityMode)


    let workerNodeName = ConfigKey "WorkerNodeName"
    let workerNodeServiceAddress = ConfigKey "WorkerNodeServiceAddress"
    let workerNodeServiceHttpPort = ConfigKey "WorkerNodeServiceHttpPort"
    let workerNodeServiceNetTcpPort = ConfigKey "WorkerNodeServiceNetTcpPort"
    let workerNodeServiceCommunicationType = ConfigKey "WorkerNodeServiceCommunicationType"
    let workerNodeId = ConfigKey "WorkerNodeId"

    let noOfCores = ConfigKey "NoOfCores"
    let isInactive = ConfigKey "IsInactive"
    let nodePriority = ConfigKey "NodePriority"


    let loadWorkerNodeServiceSettings providerRes =
        let workerNodeServiceAddress = getServiceAddress providerRes workerNodeServiceAddress defaultWorkerNodeServiceAddress
        let workerNodeServiceHttpPort = getServiceHttpPort providerRes workerNodeServiceHttpPort defaultWorkerNodeHttpServicePort
        let workerNodeServiceNetTcpPort = getServiceNetTcpPort providerRes workerNodeServiceNetTcpPort defaultWorkerNodeNetTcpServicePort
        let workerNodeServiceCommunicationType = getCommunicationType providerRes workerNodeServiceCommunicationType NetTcpCommunication

        let workerNodeSvcInfo =
            WorkerNodeServiceAccessInfo.create workerNodeServiceAddress workerNodeServiceHttpPort workerNodeServiceNetTcpPort WcfSecurityMode.defaultValue

        (workerNodeSvcInfo, workerNodeServiceCommunicationType)


    let tryGetWorkerNodeId (providerRes : AppSettingsProviderResult) n =
        match providerRes with
        | Ok provider ->
            match provider.tryGetGuid n with
            | Ok (Some p) when p <> Guid.Empty -> p |> MessagingClientId |> WorkerNodeId |> Some
            | _ -> None
        | _ -> None


    let tryGetWorkerNodeName (providerRes : AppSettingsProviderResult) n =
        match providerRes with
        | Ok provider ->
            match provider.tryGetString n with
            | Ok (Some EmptyString) -> None
            | Ok (Some s) -> s |> WorkerNodeName |> Some
            | _ -> None
        | _ -> None


    let getNoOfCores (providerRes : AppSettingsProviderResult) n d =
        match providerRes with
        | Ok provider ->
            match provider.tryGetInt n with
            | Ok (Some k) when k >= 0 -> k
            | _ -> d
        | _ -> d


    let getNodePriority (providerRes : AppSettingsProviderResult) n d =
        match providerRes with
        | Ok provider ->
            match provider.tryGetInt n with
            | Ok (Some k) when k >= 0 -> k
            | _ -> d
        | _ -> d
        |> WorkerNodePriority


    let getIsInactive (providerRes : AppSettingsProviderResult) n d =
        match providerRes with
        | Ok provider ->
            match provider.tryGetBool n with
            | Ok (Some b) -> b
            | _ -> d
        | _ -> d


    let tryLoadWorkerNodeInfo (providerRes : AppSettingsProviderResult) nodeIdOpt nameOpt =
        let io = nodeIdOpt |> Option.orElseWith (fun () -> tryGetWorkerNodeId providerRes workerNodeId)
        let no = nameOpt |> Option.orElseWith (fun () -> tryGetWorkerNodeName providerRes workerNodeName)

        match io, no with
        | Some i, Some n ->
            let defaultNoOfCores =
                match Environment.ProcessorCount with
                | 1 -> 1
                | 2 -> 1
                | 3 -> 2
                | _ -> (Environment.ProcessorCount / 2) + 1

            let w =
                {
                    workerNodeId = i
                    workerNodeName  = n
                    partitionerId = getPartitionerId providerRes partitionerId defaultPartitionerId
                    noOfCores = getNoOfCores providerRes noOfCores defaultNoOfCores
                    nodePriority = getNodePriority providerRes nodePriority WorkerNodePriority.defaultValue.value
                    isInactive = getIsInactive providerRes isInactive true
                    lastErrorDateOpt = None
                }

            Some w
        | _ -> None


    /// Type parameter 'P is needed because this class is shared by WorkerNodeService and WorkerNodeAdm
    /// and they do have different type of this 'P.
    type WorkerNodeSettingsProxy<'P> =
        {
            tryGetClientId : 'P -> WorkerNodeId option
            tryGetNodeName : 'P -> WorkerNodeName option
            tryGetPartitioner : 'P -> PartitionerId option
            tryGetNoOfCores : 'P -> int option
            tryGetInactive : 'P -> bool option
            tryGetServiceAddress : 'P -> ServiceAddress option
            tryGetServicePort : 'P -> ServicePort option
            tryGetMsgServiceAddress : 'P -> ServiceAddress option
            tryGetMsgServicePort : 'P -> ServicePort option
            tryGetForce : 'P -> bool option // TODO kk:20211123 - Not yet fully implemented.
        }


    let tryLoadWorkerNodeSettings nodeIdOpt nameOpt =
        let providerRes = AppSettingsProvider.tryCreate appSettingsFile
        let workerNodeSvcInfo, workerNodeServiceCommunicationType = loadWorkerNodeServiceSettings providerRes
        let messagingSvcInfo, messagingServiceCommunicationType = loadMessagingSettings providerRes

        match tryLoadWorkerNodeInfo providerRes nodeIdOpt nameOpt with
        | Some info ->
            let w =
                {
                    workerNodeInfo = info
                    workerNodeSvcInfo = workerNodeSvcInfo
                    workerNodeCommunicationType = workerNodeServiceCommunicationType
                    messagingSvcInfo = messagingSvcInfo
                    messagingCommunicationType = messagingServiceCommunicationType
                }

            Some w
        | None -> None


    let tryLoadSettings (proxy : WorkerNodeSettingsProxy<'P>) (p : 'P) =
        let workerNodeId = proxy.tryGetClientId p
        let workerNodeName = proxy.tryGetNodeName p

        match tryLoadWorkerNodeSettings workerNodeId workerNodeName with
        | Some w ->
            let wn = w.workerNodeSvcInfo.value.netTcpServiceInfo
            let mn = w.messagingSvcInfo.messagingServiceAccessInfo.netTcpServiceInfo

            let w1 =
                {
                    workerNodeInfo =
                        { w.workerNodeInfo with
                            partitionerId = proxy.tryGetPartitioner p |> Option.defaultValue w.workerNodeInfo.partitionerId

                            noOfCores =
                                let n = proxy.tryGetNoOfCores p |> Option.defaultValue w.workerNodeInfo.noOfCores
                                max 0 (min n (8 * Environment.ProcessorCount))

                            nodePriority =
                                match w.workerNodeInfo.nodePriority.value with
                                | x when x <= 0 -> WorkerNodePriority.defaultValue
                                | _ -> w.workerNodeInfo.nodePriority

                            isInactive = proxy.tryGetInactive p |> Option.defaultValue w.workerNodeInfo.isInactive
                            lastErrorDateOpt = w.workerNodeInfo.lastErrorDateOpt
                        }

                    workerNodeSvcInfo =
                        { w.workerNodeSvcInfo.value with
                            netTcpServiceInfo =
                                { wn with
                                    netTcpServiceAddress = proxy.tryGetServiceAddress p |> Option.defaultValue wn.netTcpServiceAddress
                                    netTcpServicePort = proxy.tryGetServicePort p |> Option.defaultValue wn.netTcpServicePort
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
                                            netTcpServiceAddress = proxy.tryGetMsgServiceAddress p |> Option.defaultValue mn.netTcpServiceAddress
                                            netTcpServicePort = proxy.tryGetMsgServicePort p |> Option.defaultValue mn.netTcpServicePort
                                        }
                                }
                            messagingDataVersion = messagingDataVersion
                        }

                    messagingCommunicationType = w.messagingCommunicationType
                }

            Some w1
        | None -> None


    let updateWorkerNodeServiceSettings (provider : AppSettingsProvider) (w : WorkerNodeServiceAccessInfo) (ct : WcfCommunicationType)  =
        let h = w.value.httpServiceInfo
        let n = w.value.netTcpServiceInfo

        provider.trySet workerNodeServiceAddress n.netTcpServiceAddress.value |> ignore
        provider.trySet workerNodeServiceHttpPort h.httpServicePort.value |> ignore
        provider.trySet workerNodeServiceNetTcpPort n.netTcpServicePort.value |> ignore
        provider.trySet workerNodeServiceCommunicationType ct.value |> ignore


    type WorkerNodeSettings
        with
        member w.trySaveSettings() =
            let toErr e = e |> WrkSettingExn |> WrkSettingsErr |> WorkerNodeErr |> Error

            match w.isValid(), AppSettingsProvider.tryCreate appSettingsFile with
            | Ok(), Ok provider ->
                let v = w.workerNodeInfo
                let wh = w.workerNodeSvcInfo.value.httpServiceInfo
                let wn = w.workerNodeSvcInfo.value.netTcpServiceInfo

                try
                    provider.trySet workerNodeName v.workerNodeName.value |> ignore
                    provider.trySet workerNodeId v.workerNodeId.value.value |> ignore
                    provider.trySet noOfCores v.noOfCores |> ignore
                    provider.trySet partitionerId v.partitionerId.value.value |> ignore
                    provider.trySet isInactive v.isInactive |> ignore
                    provider.trySet nodePriority v.nodePriority.value |> ignore

                    updateWorkerNodeServiceSettings provider w.workerNodeSvcInfo w.workerNodeCommunicationType
                    updateMessagingSettings provider w.messagingSvcInfo w.messagingCommunicationType

                    provider.trySave() |> Rop.bindError toErr
                with
                | e -> toErr e
            | Error e, _ -> Error e
            | _, Error e -> toErr e


    let getWorkerNodeServiceAccessInfo (loadSettings, tryGetSaveSettings) b =
        let w = loadSettings()
        printfn $"getServiceAccessInfoImpl: w1 = %A{w}"

        let g() =
            {
                workerNodeInfo = w.workerNodeInfo
                workerNodeServiceAccessInfo = w.workerNodeSvcInfo
                messagingServiceAccessInfo =  w.messagingSvcInfo
            }

        let r =
            match tryGetSaveSettings(), b with
            | Some(), _ -> w.trySaveSettings()
            | _, true -> w.trySaveSettings()
            | _ -> w.isValid()

        printfn $"getServiceAccessInfoImpl: r = %A{r}"

        match r with
        | Ok() -> g() |> Ok
        | Error e -> Error e
