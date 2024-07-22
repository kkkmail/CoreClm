namespace WorkerNodeService

open Argu
open ClmSys.ClmErrors
open ClmSys.WorkerNodeData
open WorkerNodeService.SvcCommandLine
open ClmSys.ClmWorker
open Softellect.Sys.Worker

module ServiceTasks =


    let getParams logger (p : ParseResults<WorkerNodeServiceRunArgs>) = getServiceAccessInfo (p.GetAllResults())
    let getSaveSettings (p : ParseResults<WorkerNodeServiceRunArgs>) () = p.GetAllResults() |> saveSettings
    type WorkerNodeServiceTask = WorkerTask<ClmResult<WorkerNodeServiceInfo>, WorkerNodeServiceRunArgs>
