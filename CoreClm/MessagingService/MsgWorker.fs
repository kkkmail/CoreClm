namespace MessagingService

open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

open Softellect.Wcf.Common
open Softellect.Wcf.Service
open MessagingService.ServiceImplementation
open MessagingServiceInfo.ServiceInfo

type MsgWorker(logger: ILogger<MsgWorker>) =
    inherit BackgroundService()

    static let tyGetHost() =
        match messagingServiceData.Value with
        | Ok data ->
            let service = MessagingWcfServiceImpl.tryGetService data
            MessagingService.tryStart() |> ignore
            service
        | Error e -> Error e

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
