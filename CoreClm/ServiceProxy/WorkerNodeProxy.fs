namespace ServiceProxy

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
            loadAllActiveRunQueueId : unit -> ClmResult<list<RunQueueId>>
            logCrit : SolverRunnerCriticalError -> UnitResult
        }

        static member create c sr =
            {
                onProcessMessageProxy =
                    {
                        saveWorkerNodeRunModelData = saveRunQueue c
                        requestCancellation = tryRequestCancelRunQueue c
                        notifyOfResults = fun q r -> tryNotifyRunQueue c q (Some r)
                        onRunModel = sr
                    }

                loadAllActiveRunQueueId = fun () -> loadAllActiveRunQueueId c
                logCrit = saveSolverRunnerErrFs name
            }

    type OnRegisterProxy =
        {
            workerNodeInfo : WorkerNodeInfo
            sendMessageProxy : SendMessageProxy
        }


    type OnStartProxy =
        {
            loadAllActiveRunQueueId : unit -> ClmResult<list<RunQueueId>>
            onRunModel : RunQueueId -> UnitResult
        }


//    type OnRunModelProxy =
//        {
//            workerNodeId : WorkerNodeId
//            runSolver : RunQueueId -> UnitResult
//            sendMessageProxy : SendMessageProxy
//            tryGetRunningSolversCount : unit -> ClmResult<int>
//            tryDeleteWorkerNodeRunModelData : RunQueueId -> UnitResult
//        }
