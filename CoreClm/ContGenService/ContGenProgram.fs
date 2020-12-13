namespace ContGenService

open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting

open Argu
open ContGenServiceInfo.ServiceInfo
open ContGenService.SvcCommandLine
open ContGenService.ContGenServiceTasks
open ContGenService.ContGenWcfService
open ClmSys.ExitErrorCodes
open ClmSys.Logging

module Program =

    //[<EntryPoint>]
    //let main (argv : string[]) : int =
    //    try
    //        let logger = Logger.log4net
    //        let parser = ArgumentParser.Create<ContGenSvcArguArgs>(programName = contGenServiceProgramName)
    //        let results = (parser.Parse argv).GetAllResults() |> ContGenSvcArgs.fromArgu convertArgs

    //        match ContGenServiceTask.tryCreate (getParams logger) getSaveSettings results with
    //        | Some task -> task.run serviceInfo |> ignore
    //        | None -> ServiceBase.Run [| new ContGenWindowsService() :> ServiceBase |]
    //        CompletedSuccessfully

    //    with
    //    | exn ->
    //        printfn "%s" exn.Message
    //        UnknownException



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
