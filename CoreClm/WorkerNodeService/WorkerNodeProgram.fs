namespace WorkerNodeService

open System.ServiceProcess
open Argu
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.SvcCommandLine
open WorkerNodeService.ServiceTasks
open WorkerNodeService.WindowsService
open ClmSys.ExitErrorCodes

module Program =

    [<EntryPoint>]
    let main (argv : string[]) : int =
        try
            let parser = ArgumentParser.Create<WorkerNodeServiceArguArgs>(programName = workerNodeServiceProgramName)
            let results = (parser.Parse argv).GetAllResults() |> WorkerNodeServiceArgs.fromArgu convertArgs

            match WorkerNodeServiceTask.tryCreate getParams getSaveSettings results with
            | Some task -> task.run serviceInfo |> ignore
            | None -> ServiceBase.Run [| new WorkerNodeWindowsService() :> ServiceBase |]

            CompletedSuccessfully

        with
        | e ->
            printfn "%s" e.Message
            UnknownException
