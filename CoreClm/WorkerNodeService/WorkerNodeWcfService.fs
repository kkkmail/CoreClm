namespace WorkerNodeService

open System
open Argu

open Softellect.Sys
open Softellect.Wcf.Service

open ClmSys.Logging
open ClmSys.WorkerNodeData
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.ServiceImplementation
open WorkerNodeService.SvcCommandLine
open ClmSys.WorkerNodePrimitives
open ClmSys.ClmErrors
open System.ServiceModel
open ClmSys
open ClmSys.WorkerNodeErrors

module WorkerNodeWcfService =

//    let private serviceName = workerNodeServiceName


    let private workerNodeRunner : Lazy<ClmResult<WorkerNodeRunner>> =
        new Lazy<ClmResult<WorkerNodeRunner>>(fun () -> WorkerNodeRunner.create serviceAccessInfo)


    let tryStartWorkerNodeRunner() =
        match workerNodeRunner.Value with
        | Ok service -> service.start() |> Ok
        | Error e -> Error e


    type WorkerNodeWcfService() =
        let toConfigureError f = f |> ConfigureWcfErr |> WorkerNodeWcfErr |> WorkerNodeServiceErr
        let toMonitorError f = f |> MonitorWcfErr |> WorkerNodeWcfErr |> WorkerNodeServiceErr
        let toPingError f = f |> PingWcfErr |> WorkerNodeWcfErr |> WorkerNodeServiceErr

        let configure c = workerNodeRunner.Value |> Rop.bind (fun e -> e.configure c)
        let monitor (_ : WorkerNodeMonitorParam) = workerNodeRunner.Value |> Rop.bind (fun e -> e.getState() |> Ok)
        let ping () = workerNodeRunner.Value |> Rop.bind (fun _ -> Ok())

        interface IWorkerNodeWcfService with
            member _.configure b = tryReply configure toConfigureError b
            member _.monitor b = tryReply monitor toMonitorError b
            member _.ping b = tryReply ping toPingError b


    type WorkerNodeWcfServiceImpl = WcfService<WorkerNodeWcfService, IWorkerNodeWcfService, WorkerNodeServiceInfo>
