namespace MessagingService

open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

type MsgWorker(logger: ILogger<MsgWorker>) =
    inherit BackgroundService()

    static let hostRes =
        match echoWcfServiceDataRes with
        | Ok data -> EchoWcfServiceImpl.tryGetService data
        | Error e -> Error e

    override _.ExecuteAsync(_: CancellationToken) =
        async {
            logger.LogInformation("Executing...")

            match hostRes with
            | Ok host -> do! host.runAsync()
            | Error e -> logger.LogCritical(sprintf "Error: %A" e)
        }
        |> Async.StartAsTask
        :> Task

    override _.StopAsync(_: CancellationToken) =
        async {
            logger.LogInformation("Stopping...")

            match hostRes with
            | Ok host -> do! host.stopAsync()
            | Error e -> logger.LogCritical(sprintf "Error: %A" e)
        }
        |> Async.StartAsTask
        :> Task
