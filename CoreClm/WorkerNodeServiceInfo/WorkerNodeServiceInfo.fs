namespace WorkerNodeServiceInfo

open System
open System.Threading

open Softellect.Sys
open Softellect.Sys.Core
open Softellect.Sys.Primitives
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.Logging
open Softellect.Sys.WcfErrors
open Softellect.Sys.MessagingErrors
open Softellect.Sys.AppSettings
open Softellect.Wcf.Common
open Softellect.Wcf.Client
open Softellect.Wcf.Service
open Softellect.Messaging.Primitives
open Softellect.Messaging.ServiceInfo
open Softellect.Messaging.Service
open Softellect.Messaging.Client
open Softellect.Messaging.Proxy
open Softellect.Sys.MessagingClientErrors
open Softellect.Sys.MessagingServiceErrors

open ClmSys.GeneralData
open ClmSys.ClmErrors
open ClmSys.GeneralPrimitives
open ClmSys.ContGenPrimitives
open ClmSys.SolverRunnerPrimitives
open ClmSys.WorkerNodeData
open System.ServiceModel
open ClmSys.WorkerNodeErrors
open ClmSys.MessagingPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.PartitionerData
open MessagingServiceInfo.ServiceInfo
open ContGenServiceInfo.ServiceInfo

module ServiceInfo =

    let workerNodeServiceProgramName = "WorkerNodeService.exe"


    [<Literal>]
    let WorkerNodeWcfServiceName = "WorkerNodeWcfService"


//    type WrkNodeWcfSvcShutDownInfo =
//        {
//            wrkNodeServiceHost : ServiceHost
//        }


    type WorkerNodeConfigParam =
        | WorkerNumberOfSores of int


    type RunnerState =
        {
            progress : TaskProgress
            started : DateTime
            lastUpdated : DateTime
        }

        static member defaultValue =
            {
                progress = NotStarted
                started = DateTime.Now
                lastUpdated = DateTime.Now
            }

        override r.ToString() =
            let s = (DateTime.Now - r.started).ToString("d\.hh\:mm")

            let estCompl =
                match r.progress.estimateEndTime r.started with
                | Some e -> " ETC: " + e.ToString("yyyy-MM-dd.HH:mm") + ";"
                | None -> EmptyString

            sprintf "T: %s;%s %A" s estCompl r.progress


    type RunnerStateWithCancellation =
        {
            runnerState : RunnerState
            cancellationTypeOpt : CancellationType option
            notifyOfResults : ResultNotificationType -> UnitResult
        }

        static member defaultValue n =
            {
                runnerState = RunnerState.defaultValue
                cancellationTypeOpt = None
                notifyOfResults = n
            }


    type WorkerNodeRunnerMonitorState =
        {
            workers : Map<RunQueueId, RunnerState>
            noOfWorkerCores : int
        }


    type WorkerNodeState =
        | NotStartedWorkerNode
        | StartedWorkerNode

    type WorkerNodeRunnerState =
        {
            runningWorkers : Map<RunQueueId, RunnerStateWithCancellation>
            numberOfWorkerCores : int
            workerNodeState : WorkerNodeState
        }

        member w.toWorkerNodeRunnerMonitorState() =
            {
                workers = w.runningWorkers |> Map.map (fun _ e -> e.runnerState)
                noOfWorkerCores = w.numberOfWorkerCores
            }


    type WorkerNodeRunnerResult = StateWithResult<WorkerNodeRunnerState>


    type WorkerNodeMonitorParam =
        | DummyWrkMonitorParam


    type WorkerNodeMonitorResponse =
        | CannotAccessWrkNode
        | ErrorOccurred of ClmError
        | WrkNodeState of WorkerNodeRunnerMonitorState

        override this.ToString() =
            match this with
            | CannotAccessWrkNode -> "Cannot access worker node."
            | WrkNodeState s ->
                let toString acc ((RunQueueId k), (v : RunnerState)) =
                    acc + (sprintf "        Q: %A; %s; L: %s\n" k (v.ToString()) (v.lastUpdated.ToString("yyyy-MM-dd.HH:mm")))

                let x =
                    match s.workers |> Map.toList |> List.sortBy (fun (_, r) -> r.progress) |> List.fold toString EmptyString with
                    | EmptyString -> "[]"
                    | s -> "\n    [\n" + s + "    ]"
                sprintf "Running: %s\nCount: %A, cores: %A" x s.workers.Count s.noOfWorkerCores
            | ErrorOccurred e -> "Error occurred: " + e.ToString()


    type IWorkerNodeService =
        abstract configure : WorkerNodeConfigParam -> UnitResult
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
            | e -> printfn "Exception occurred: %A" e
        else
            printfn "Not getting state at %A because callCount = %A." DateTime.Now callCount
            ignore()

        Interlocked.Decrement(&callCount) |> ignore


    /// https://gist.github.com/dgfitch/661656
    [<ServiceContract(ConfigurationName = WorkerNodeWcfServiceName)>]
    type IWorkerNodeWcfService =

        [<OperationContract(Name = "configure")>]
        abstract configure : q:byte[] -> byte[]

        [<OperationContract(Name = "monitor")>]
        abstract monitor : q:byte[] -> byte[]

        [<OperationContract(Name = "ping")>]
        abstract ping : q:byte[] -> byte[]


    /// Low level WCF messaging client.
    type WorkerNodeResponseHandler private (url, communicationType) =
        let tryGetWcfService() = tryGetWcfService<IWorkerNodeWcfService> communicationType url

        let configureWcfErr e = e |> ConfigureWcfErr |> WorkerNodeWcfErr |> WorkerNodeServiceErr
        let monitorWcfErr e = e |> MonitorWcfErr |> WorkerNodeWcfErr |> WorkerNodeServiceErr
        let pingWcfErr e = e |> PingWcfErr |> WorkerNodeWcfErr |> WorkerNodeServiceErr

        let configureImpl p = tryCommunicate tryGetWcfService (fun service -> service.configure) configureWcfErr p
        let monitorImpl p = tryCommunicate tryGetWcfService (fun service -> service.monitor) monitorWcfErr p
        let pingImpl() = tryCommunicate tryGetWcfService (fun service -> service.ping) pingWcfErr ()

        interface IWorkerNodeService with
            member _.configure p = configureImpl p
            member _.monitor p = monitorImpl p
            member _.ping() = pingImpl()

        new (i : WorkerNodeServiceAccessInfo, communicationType) = WorkerNodeResponseHandler(i.value.getUrl communicationType, communicationType)


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
        let workerNodeServiceAddress = getServiceAddress providerRes workerNodeServiceAddress defaultContGenServiceAddress
        let workerNodeServiceHttpPort = getServiceHttpPort providerRes workerNodeServiceHttpPort defaultContGenHttpServicePort
        let workerNodeServiceNetTcpPort = getServiceNetTcpPort providerRes workerNodeServiceNetTcpPort defaultContGenNetTcpServicePort
        let workerNodeServiceCommunicationType = getCommunicationType providerRes workerNodeServiceCommunicationType NetTcpCommunication

        let workerNodeSvcInfo = WorkerNodeServiceAccessInfo.create workerNodeServiceAddress workerNodeServiceHttpPort workerNodeServiceNetTcpPort

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


    let tryLoadWorkerNodeSettings nodeIdOpt nameOpt =
        let providerRes = AppSettingsProvider.tryCreate appSettingsFile
        let (workerNodeSvcInfo, workerNodeServiceCommunicationType) = loadWorkerNodeServiceSettings providerRes
        let (messagingSvcInfo, messagingServiceCommunicationType) = loadMessagingSettings providerRes

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
        printfn "getServiceAccessInfoImpl: w1 = %A" w

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

        printfn "getServiceAccessInfoImpl: r = %A" r

        match r with
        | Ok() -> g() |> Ok
        | Error e -> Error e
