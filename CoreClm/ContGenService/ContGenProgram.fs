namespace ContGenService

open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting

open Argu
open ContGenServiceInfo.ServiceInfo
open ContGenService.SvcCommandLine
open ContGenService.ContGenServiceTasks
open ClmSys.ExitErrorCodes
open ClmSys.Logging

module Program =

    let createHostBuilder() =
        Host.CreateDefaultBuilder()
            .UseWindowsService()
            .ConfigureServices(fun hostContext services -> services.AddHostedService<ContGenWorker>() |> ignore)


    [<EntryPoint>]
    let main argv =
        let runHost() = createHostBuilder().Build().Run()

        try
            let parser = ArgumentParser.Create<ContGenSvcArguArgs>(programName = contGenServiceProgramName)
            let results = (parser.Parse argv).GetAllResults() |> ContGenSvcArgs.fromArgu convertArgs

            let run p =
                getParams Logger.defaultValue p |> ignore
                runHost

            match ContGenServiceTask.tryCreate run getSaveSettings results with
            | Some task -> task.run()
            | None ->  runHost()

            CompletedSuccessfully

        with
        | exn ->
            printfn "%s" exn.Message
            UnknownException
