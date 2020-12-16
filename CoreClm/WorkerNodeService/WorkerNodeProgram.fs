namespace WorkerNodeService

open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting

open Argu
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.SvcCommandLine
open WorkerNodeService.ServiceTasks
open WorkerNodeService.WorkerNodeWcfService
open WorkerNodeService
open ClmSys.ExitErrorCodes
open ClmSys.Logging

module Program =

    //[<EntryPoint>]
    //let main (argv : string[]) : int =
    //    try
    //        let parser = ArgumentParser.Create<WorkerNodeServiceArguArgs>(programName = workerNodeServiceProgramName)
    //        let results = (parser.Parse argv).GetAllResults() |> WorkerNodeServiceArgs.fromArgu convertArgs

    //        match WorkerNodeServiceTask.tryCreate getParams getSaveSettings results with
    //        | Some task -> task.run serviceInfo |> ignore
    //        | None -> ServiceBase.Run [| new WorkerNodeWindowsService() :> ServiceBase |]

    //        CompletedSuccessfully

    //    with
    //    | e ->
    //        printfn "%s" e.Message
    //        UnknownException

    let createHostBuilder() =
        Host.CreateDefaultBuilder()
            .UseWindowsService()
            .ConfigureServices(fun hostContext services -> services.AddHostedService<WorkerNodeWorker>() |> ignore)


    [<EntryPoint>]
    let main argv =
        let runHost() = createHostBuilder().Build().Run()

        try
            let parser = ArgumentParser.Create<WorkerNodeServiceArguArgs>(programName = workerNodeServiceProgramName)
            let results = (parser.Parse argv).GetAllResults() |> WorkerNodeServiceArgs.fromArgu convertArgs

            let run p =
                getParams Logger.defaultValue p |> ignore
                runHost

            match WorkerNodeServiceTask.tryCreate run getSaveSettings results with
            | Some task -> task.run()
            | None -> runHost()


            CompletedSuccessfully

        with
        | exn ->
            printfn "%s" exn.Message
            UnknownException

