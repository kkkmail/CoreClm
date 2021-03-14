namespace MessagingService

open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting

open Argu
open MessagingServiceInfo.ServiceInfo
open MessagingService.SvcCommandLine
open MessagingService.ServiceTasks
open ClmSys.ExitErrorCodes

module Program =

    let createHostBuilder() =
        Host.CreateDefaultBuilder()
            .UseWindowsService()
            .ConfigureServices(fun hostContext services -> services.AddHostedService<MsgWorker>() |> ignore)


    [<EntryPoint>]
    let main argv =
        let runHost() = createHostBuilder().Build().Run()

        try
            let parser = ArgumentParser.Create<MsgSvcArguArgs>(programName = messagingProgramName)
            let results = (parser.Parse argv).GetAllResults() |> MsgSvcArgs.fromArgu convertArgs

            let run p =
                getParams p |> ignore
                runHost

            match MessagingServiceTask.tryCreate run getSaveSettings results with
            | Some task -> task.run()
            | None ->  runHost()

            CompletedSuccessfully

        with
        | exn ->
            printfn $"%s{exn.Message}"
            UnknownException
