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
open DbData.Configuration
open DbData.WorkerNodeDatabaseTypes

module WorkerNodeProxy =

    let private name = WorkerNodeServiceName.netTcpServiceName.value.value |> MessagingClientName


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


    type OnProcessMessageProxy =
        {
            saveWorkerNodeRunModelData : WorkerNodeRunModelData -> UnitResult
            requestCancellation : RunQueueId -> CancellationType -> UnitResult
            notifyOfResults : RunQueueId -> ResultNotificationType -> UnitResult
            onRunModel : RunQueueId -> UnitResult
        }


    type WorkerNodeProxy =
        {
            onProcessMessageProxy : OnProcessMessageProxy
            loadAllActiveRunQueueId : unit -> ListResult<RunQueueId>
            logCrit : SolverRunnerCriticalError -> UnitResult
        }

//        static member createLocal (i : WorkerNodeProxyData) =
//            let name = WorkerNodeServiceName.netTcpServiceName.value.value |> MessagingClientName
//
//            {
//                saveWorkerNodeRunModelData = saveWorkerNodeRunModelDataFs name
//                loadWorkerNodeRunModelData = loadWorkerNodeRunModelDataFs name
//                tryDeleteWorkerNodeRunModelData = tryDeleteWorkerNodeRunModelDataFs name
//                loadAllWorkerNodeRunModelData = loadWorkerNodeRunModelDataAllFs name
//                logCrit = saveSolverRunnerErrFs name
//            }

        static member create c sr =
            {
                onProcessMessageProxy =
                    {
                        saveWorkerNodeRunModelData = saveRunQueue c
                        requestCancellation = tryRequestCancelRunQueue c
                        notifyOfResults = fun q r -> tryNotifyRunQueue c q (Some r)
                        onRunModel = sr
                    }

                loadAllActiveRunQueueId = loadAllActiveRunQueueId c
                logCrit = saveSolverRunnerErrFs name
            }

    type OnRegisterProxy =
        {
            workerNodeInfo : WorkerNodeInfo
            sendMessageProxy : SendMessageProxy
        }


    type OnStartProxy =
        {
            loadAllActiveRunQueueId : unit -> ListResult<RunQueueId>
            onRunModel : RunQueueId -> UnitResult
            noOfCores : int
        }


    type OnRunModelProxy =
        {
            workerNodeId : WorkerNodeId
            numberOfWorkerCores : int
            runSolver : RunQueueId -> UnitResult
            sendMessageProxy : SendMessageProxy
            tryGetRunningSolversCount : unit -> ClmResult<int>
            tryDeleteWorkerNodeRunModelData : RunQueueId -> UnitResult
        }
