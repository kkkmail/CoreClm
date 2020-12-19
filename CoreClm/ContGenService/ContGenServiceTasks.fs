namespace ContGenService

open Argu
open ClmSys.ClmErrors
open ClmSys.ClmWorker
open ContGenService.SvcCommandLine

module ContGenServiceTasks =

    let getParams logger (p : ParseResults<ContGenRunArgs>) = tryGetContGenServiceData logger (p.GetAllResults())
    let getSaveSettings (p : ParseResults<ContGenRunArgs>) () = p.GetAllResults() |> saveSettings
    type ContGenServiceTask = WorkerTask<ClmResult<ContGenServiceData>, ContGenRunArgs>
