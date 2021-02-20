namespace ServiceProxy

open ClmSys.PartitionerPrimitives
open ClmSys.WorkerNodeData
open ServiceProxy.SolverProcessProxy
open Softellect.Messaging.Primitives

open NoSql.FileSystemTypes
open MessagingServiceInfo.ServiceInfo
open ClmSys.GeneralPrimitives
open ClmSys.SolverRunnerPrimitives
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.SolverRunnerErrors
open ClmSys.WorkerNodePrimitives
open WorkerNodeServiceInfo.ServiceInfo

module WorkerNodeProxy =

    type StorageType =
        | LocalStorage of ConnectionString
        | RemoteStorage


    type WorkerNodeProxyData =
        {
            minUsefulEe : MinUsefulEe
            noOfProgressPoints : int option
        }

        static member defaultValue =
            {
                minUsefulEe = MinUsefulEe.defaultValue
                noOfProgressPoints = None
            }

    type WorkerNodeProxy =
        {
            saveWorkerNodeRunModelData : WorkerNodeRunModelData -> UnitResult
            loadWorkerNodeRunModelData : RunQueueId -> ClmResult<WorkerNodeRunModelData>
            tryDeleteWorkerNodeRunModelData : RunQueueId -> UnitResult
            loadAllWorkerNodeRunModelData : unit -> ListResult<WorkerNodeRunModelData>
            logCrit : SolverRunnerCriticalError -> UnitResult
        }

        static member create (i : WorkerNodeProxyData) =
            let name = WorkerNodeServiceName.netTcpServiceName.value.value |> MessagingClientName

            {
                saveWorkerNodeRunModelData = saveWorkerNodeRunModelDataFs name
                loadWorkerNodeRunModelData = loadWorkerNodeRunModelDataFs name
                tryDeleteWorkerNodeRunModelData = tryDeleteWorkerNodeRunModelDataFs name
                loadAllWorkerNodeRunModelData = loadWorkerNodeRunModelDataAllFs name
                logCrit = saveSolverRunnerErrFs name
            }


    type OnRegisterProxy =
        {
            workerNodeInfo : WorkerNodeInfo
            sendMessageProxy : SendMessageProxy
        }


    type OnStartProxy =
        {
            loadAllWorkerNodeRunModelData : unit -> ListResult<RunQueueId>
            onRunModel : RunQueueId -> UnitResult
            noOfCores : int
        }


    type OnProcessMessageProxy =
        {
            saveWorkerNodeRunModelData : WorkerNodeRunModelData -> UnitResult
            requestCancellation : RunQueueId -> CancellationType -> UnitResult
            notifyOfResults : RunQueueId -> ResultNotificationType -> UnitResult
            onRunModel : RunQueueId -> UnitResult
        }


    type OnRunModelProxy =
        {
            workerNodeId : WorkerNodeId
            numberOfWorkerCores : int
            onRunModel : RunQueueId -> UnitResult
            sendMessageProxy : SendMessageProxy
            tryGetRunningSolversCount : unit -> ClmResult<int>
            tryDeleteWorkerNodeRunModelData : RunQueueId -> UnitResult
        }
