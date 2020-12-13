namespace ContGenService

open Argu
open ClmSys.ClmErrors
open ClmSys.Logging
open ContGenService.SvcCommandLine
open ClmSys.ContGenPrimitives
open ClmSys.ClmWorker

module ContGenServiceTasks =

    //let serviceInfo =
    //    {
    //        serviceName = contGenServiceName.value
    //        runService = startContGenWcfServiceRun
    //        cleanup = cleanupService
    //        timeoutMilliseconds = None
    //        logger = logger
    //    }


    let getParams logger (p : ParseResults<ContGenRunArgs>) = getContGenServiceData logger (p.GetAllResults())
    let getSaveSettings (p : ParseResults<ContGenRunArgs>) () = p.GetAllResults() |> saveSettings
    type ContGenServiceTask = WorkerTask<ClmResult<ContGenServiceData>, ContGenRunArgs>
