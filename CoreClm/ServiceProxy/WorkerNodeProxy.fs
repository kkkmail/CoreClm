namespace ServiceProxy

open NoSql.FileSystemTypes
open MessagingServiceInfo.ServiceInfo
open ClmSys.GeneralPrimitives
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.SolverRunnerErrors
open ClmSys.WorkerNodePrimitives
open ClmSys.MessagingPrimitives

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
            let name = workerNodeServiceName.value.messagingClientName

            {
                saveWorkerNodeRunModelData = saveWorkerNodeRunModelDataFs name
                loadWorkerNodeRunModelData = loadWorkerNodeRunModelDataFs name
                tryDeleteWorkerNodeRunModelData = tryDeleteWorkerNodeRunModelDataFs name
                loadAllWorkerNodeRunModelData = loadWorkerNodeRunModelDataAllFs name
                logCrit = saveSolverRunnerErrFs name
            }
