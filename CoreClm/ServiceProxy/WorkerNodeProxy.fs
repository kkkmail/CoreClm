namespace ServiceProxy

open ClmSys.PartitionerPrimitives
open ClmSys.WorkerNodeData
open Softellect.Messaging.Primitives

open NoSql.FileSystemTypes
open MessagingServiceInfo.ServiceInfo
open ClmSys.GeneralPrimitives
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


    type SendMessageProxy =
        {
            partitionerId : PartitionerId
            sendMessage : MessageInfo -> UnitResult
        }


    type OnRegisterProxy =
        {
            workerNodeInfo : WorkerNodeInfo
            sendMessageProxy : SendMessageProxy
        }


    type OnUpdateProgressProxy =
        {
            tryDeleteWorkerNodeRunModelData : RunQueueId -> UnitResult
            sendMessageProxy : SendMessageProxy
        }


//    type OnRunModelProxy =
//        {
//            workerNodeId : WorkerNodeId
//            getSolverRunner : WorkerNodeRunModelData -> SolverRunner
//            sendMessageProxy : SendMessageProxy
//            tryDeleteWorkerNodeRunModelData : RunQueueId -> UnitResult
//        }

    type OnStartProxy =
        {
            loadAllWorkerNodeRunModelData : unit -> ListResult<WorkerNodeRunModelData>
            onRunModel : WorkerNodeRunnerState -> WorkerNodeRunModelData -> WorkerNodeRunnerResult
            noOfCores : int
        }


    type OnProcessMessageProxy =
        {
            saveWorkerNodeRunModelData : WorkerNodeRunModelData -> UnitResult
            tryDeleteWorkerNodeRunModelData : RunQueueId -> UnitResult
            onRunModel : WorkerNodeRunnerState -> WorkerNodeRunModelData -> WorkerNodeRunnerResult
        }



