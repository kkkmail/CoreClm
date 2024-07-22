namespace ServiceProxy

open ClmSys.WorkerNodeData
open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives
open Primitives.SolverRunnerErrors
open ServiceProxy.SolverProcessProxy
open Softellect.Messaging.Primitives

open NoSql.FileSystemTypes
open MessagingServiceInfo.ServiceInfo
open ClmSys.GeneralPrimitives
open ClmSys.ClmErrors
open ClmSys.WorkerNodePrimitives
open DbData.WorkerNodeDatabaseTypes
open Softellect.Sys.Primitives

module WorkerNodeProxy =

    let private name = WorkerNodeServiceName.netTcpServiceName.value.value |> MessagingClientName


    type StorageType =
        | LocalStorage of ConnectionString
        | RemoteStorage


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
