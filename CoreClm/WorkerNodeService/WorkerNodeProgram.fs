namespace WorkerNodeService

open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting

open Argu
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.SvcCommandLine
open WorkerNodeService.ServiceTasks
open WorkerNodeService
open ClmSys.ExitErrorCodes
open ClmSys.Logging

module Program =

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
            printfn $"%s{exn.Message}"
            UnknownException

