﻿namespace WorkerNodeService

open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

open Softellect.Sys.Logging
open Softellect.Wcf.Common
open Softellect.Wcf.Service
open Softellect.Sys.WcfErrors

open WorkerNodeService.WorkerNodeWcfService
open WorkerNodeService.ServiceImplementation

type WorkerNodeWorker(logger: ILogger<WorkerNodeWorker>) =
    inherit BackgroundService()

    static let tyGetHost() =
        let logger =  Logger.defaultValue

        match serviceAccessInfo with
        | Ok data ->
            match tryGetServiceData data.workerNodeServiceAccessInfo.value logger data with
            | Ok serviceData ->
                let service = WorkerNodeWcfServiceImpl.tryGetService serviceData
                let r = tryStartWorkerNodeRunner()
                service
            | Error e -> Error e
        | Error e ->
            // TODO kk:20201213 - Here we are forced to "downgrade" the error type because there is no conversion.
            e.ToString() |> WcfCriticalErr |> Error

    static let hostRes = Lazy<WcfResult<WcfService>>(fun () -> tyGetHost())

    //override _.StartAsync(token: CancellationToken) =
    //    async {
    //        printfn "WorkerNodeWorker::Starting..."
    //        //base.StartAsync(token)
    //        logger.LogInformation("Starting...")

    //        match hostRes.Value with
    //        | Ok host -> do! host.runAsync()
    //        | Error e -> logger.LogCritical$"Error: %A{e}"
    //    }
    //    |> Async.StartAsTask
    //    :> Task

    override _.ExecuteAsync(_: CancellationToken) =
        async {
            printfn "WorkerNodeWorker::Executing..."
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
