namespace WorkerNodeServiceInfo

open System
open System.Threading
open FSharp.Configuration
open ClmSys.GeneralData
open ClmSys.ClmErrors
open ClmSys.GeneralPrimitives
open ClmSys.ContGenPrimitives
open ClmSys.SolverRunnerPrimitives
open ClmSys.WorkerNodeData
open System.ServiceModel
open ClmSys.Wcf
open ClmSys.WorkerNodeErrors
open ClmSys.MessagingPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.PartitionerData

module ServiceInfo =

    let workerNodeServiceProgramName = "WorkerNodeService.exe"


    [<Literal>]
    let WorkerNodeWcfServiceName = "WorkerNodeWcfService"


    type WrkNodeWcfSvcShutDownInfo =
        {
            wrkNodeServiceHost : ServiceHost
        }


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
    type WorkerNodeResponseHandler private (url) =
        let tryGetWcfService() = tryGetWcfService<IWorkerNodeWcfService> url

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

        new (i : WorkerNodeServiceAccessInfo) = WorkerNodeResponseHandler(i.wcfServiceUrl)


    [<Literal>]
    let WorkerNodeAppConfigFile = __SOURCE_DIRECTORY__ + @"\..\WorkerNodeService\app.config"


    type WorkerNodeAppSettings = AppSettings<WorkerNodeAppConfigFile>


    let loadWorkerNodeSettings() =
        WorkerNodeAppSettings.SelectExecutableFile(getFileName workerNodeServiceProgramName)

        {
            workerNodeInfo =
                {
                    workerNodeId = WorkerNodeAppSettings.WorkerNodeId |> MessagingClientId |> WorkerNodeId
                    workerNodeName  = WorkerNodeAppSettings.WorkerNodeName |> WorkerNodeName

                    partitionerId =
                        match WorkerNodeAppSettings.PartitionerId with
                        | p when p <> Guid.Empty -> p |> MessagingClientId |> PartitionerId
                        | _ -> defaultPartitionerId

                    noOfCores = WorkerNodeAppSettings.NoOfCores
                    nodePriority = WorkerNodeAppSettings.NodePriority |> WorkerNodePriority
                    isInactive = WorkerNodeAppSettings.IsInactive
                    lastErrorDateOpt = None
                }

            workerNodeSvcInfo =
                {
                    workerNodeServiceAddress =
                        match WorkerNodeAppSettings.WorkerNodeSvcAddress with
                        | EmptyString -> WorkerNodeServiceAddress.defaultValue
                        | s -> s |> ServiceAddress |> WorkerNodeServiceAddress

                    workerNodeServicePort =
                        match WorkerNodeAppSettings.WorkerNodeSvcPort with
                        | n when n > 0 -> n |> ServicePort |> WorkerNodeServicePort
                        | _ -> WorkerNodeServicePort.defaultValue

                    workerNodeServiceName = workerNodeServiceName
                }

            messagingSvcInfo =
                {
                    messagingServiceAddress =
                        match WorkerNodeAppSettings.MsgSvcAddress with
                        | EmptyString -> MessagingServiceAddress.defaultValue
                        | s -> s |> ServiceAddress |> MessagingServiceAddress

                    messagingServicePort =
                        match WorkerNodeAppSettings.MsgSvcPort with
                        | n  when n > 0 -> n |> ServicePort |> MessagingServicePort
                        | _ -> MessagingServicePort.defaultValue

                    messagingServiceName = messagingServiceName
                }
        }


    type WorkerNodeSettings
        with
        member w.trySaveSettings() =
            match w.isValid() with
            | Ok() ->
                try
                    WorkerNodeAppSettings.WorkerNodeName <- w.workerNodeInfo.workerNodeName.value
                    WorkerNodeAppSettings.WorkerNodeId <- w.workerNodeInfo.workerNodeId.value.value
                    WorkerNodeAppSettings.NoOfCores <- w.workerNodeInfo.noOfCores
                    WorkerNodeAppSettings.PartitionerId <- w.workerNodeInfo.partitionerId.value.value
                    WorkerNodeAppSettings.IsInactive <- w.workerNodeInfo.isInactive
                    WorkerNodeAppSettings.NodePriority <- w.workerNodeInfo.nodePriority.value

                    WorkerNodeAppSettings.WorkerNodeSvcAddress <- w.workerNodeSvcInfo.workerNodeServiceAddress.value.value
                    WorkerNodeAppSettings.WorkerNodeSvcPort <- w.workerNodeSvcInfo.workerNodeServicePort.value.value

                    WorkerNodeAppSettings.MsgSvcAddress <- w.messagingSvcInfo.messagingServiceAddress.value.value
                    WorkerNodeAppSettings.MsgSvcPort <- w.messagingSvcInfo.messagingServicePort.value.value

                    Ok()
                with
                | e -> e |> WrkSettingExn |> WrkSettingsErr |> WorkerNodeErr |> Error
            | Error e -> Error e


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
