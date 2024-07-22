namespace ContGenService

open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

open Softellect.Sys.Logging
open Softellect.Wcf.Common
open Softellect.Wcf.Service
open Softellect.Wcf.Errors

open ContGenService.ContGenWcfService
open ContGenService.SvcCommandLine

type ContGenWorker(logger: ILogger<ContGenWorker>) =
    inherit BackgroundService()

    static let tyGetHost() =
        match contGenServiceData.Value with
        | Ok data ->
            match tryGetServiceData data.contGenServiceAccessInfo.value Logger.defaultValue data with
            | Ok serviceData ->
                let service = ContGenWcfServiceImpl.tryGetService serviceData
                tryStartModelRunner() |> ignore
                service
            | Error e -> Error e
        | Error e ->
            // TODO kk:20201213 - Here we are forced to "downgrade" the error type because there is no conversion.
            e.ToString() |> WcfCriticalErr |> Error

    static let hostRes = Lazy<WcfResult<WcfService>>(fun () -> tyGetHost())

    override _.ExecuteAsync(_: CancellationToken) =
        async {
            logger.LogInformation("Executing...")

            match hostRes.Value with
            | Ok host -> do! host.runAsync()
            | Error e -> logger.LogCritical$"Error: %A{e}"
        }
        |> Async.StartAsTask
        :> Task

    override _.StopAsync(_: CancellationToken) =
        async {
            logger.LogInformation("Stopping...")

            match hostRes.Value with
            | Ok host -> do! host.stopAsync()
            | Error e -> logger.LogCritical$"Error: %A{e}"
        }
        |> Async.StartAsTask
        :> Task
