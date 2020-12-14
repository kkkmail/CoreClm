namespace WorkerNodeService

open Argu
open ClmSys.ClmErrors
//open ClmSys.ServiceInstaller
open ClmSys.Logging
open ClmSys.WorkerNodeData
open WorkerNodeService.SvcCommandLine
//open WorkerNodeService.WindowsService
open ClmSys.WorkerNodePrimitives
open ClmSys.ClmWorker

module ServiceTasks =

//    let serviceInfo =
//        {
//            serviceName = workerNodeServiceName.value
//            runService = startWrkNodeWcfServiceRun
//            cleanup = cleanupService
//            timeoutMilliseconds = None
//            logger = logger
//        }


    let getParams logger (p : ParseResults<WorkerNodeServiceRunArgs>) = getServiceAccessInfo (p.GetAllResults())
    let getSaveSettings (p : ParseResults<WorkerNodeServiceRunArgs>) () = p.GetAllResults() |> saveSettings
    type WorkerNodeServiceTask = WorkerTask<ClmResult<WorkerNodeServiceInfo>, WorkerNodeServiceRunArgs>
