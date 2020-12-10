namespace WorkerNodeService

open System
open System.ServiceProcess
open Argu
open ClmSys.Logging
open ClmSys.WorkerNodeData
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.ServiceImplementation
open WorkerNodeService.SvcCommandLine
open ClmSys.WorkerNodePrimitives
open ClmSys.Wcf
open ClmSys.ClmErrors
open System.ServiceModel
open ClmSys
open ClmSys.WorkerNodeErrors

module WindowsService =

    let private serviceName = workerNodeServiceName


    let private workerNodeRunner : Lazy<ClmResult<WorkerNodeRunner>> =
        new Lazy<ClmResult<WorkerNodeRunner>>(fun () -> WorkerNodeRunner.create serviceAccessInfo)


    [<ServiceBehavior(IncludeExceptionDetailInFaults = true, InstanceContextMode = InstanceContextMode.Single)>]
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


    let startWrkNodeWcfServiceRun (logger : Logger) (j : ClmResult<WorkerNodeServiceInfo>) : WrkNodeWcfSvcShutDownInfo option =
        try
            printfn "startWrkNodeWcfServiceRun: Creating WCF Worker Node Service..."
            serviceAccessInfo <- j

            match workerNodeRunner.Value, j with
            | Ok r, Ok i ->
                match r.start() with
                | Ok() ->
                    let binding = getBinding()
                    let baseAddress = Uri(i.workerNodeServiceAccessInfo.wcfServiceUrl)
                    let serviceHost = new ServiceHost(typeof<WorkerNodeWcfService>, baseAddress)
                    let _ = serviceHost.AddServiceEndpoint(typeof<IWorkerNodeWcfService>, binding, baseAddress)
                    do serviceHost.Open()
                    printfn "startWrkNodeWcfServiceRun: Completed."

                    {
                        wrkNodeServiceHost = serviceHost
                    }
                    |> Some
                | Error e ->
                    printfn "startWrkNodeWcfServiceRun: Error - %A." e
                    None
            | Error e, Ok _ ->
                printfn "startWrkNodeWcfServiceRun: Error - %A." e
                None
            | Ok _, Error e ->
                printfn "startWrkNodeWcfServiceRun: Error - %A." e
                None
            | Error e1, Error e2 ->
                printfn "startWrkNodeWcfServiceRun: Errors - %A, %A." e1 e2
                None
        with
        | e ->
            logger.logExn "startWrkNodeWcfServiceRun: Error starting WCF Worker Node Service." e
            None


    let cleanupService (logger : Logger) (i : WrkNodeWcfSvcShutDownInfo) =
        try
            logger.logInfoString "WorkerNodeWindowsService: Closing WCF service host."
            i.wrkNodeServiceHost.Close()
        with
        | e -> logger.logExn "WorkerNodeWindowsService: Exception occurred: " e


    type public WorkerNodeWindowsService () =
        inherit ServiceBase (ServiceName = serviceName.value.value)

        let initService () = ()
        do initService ()
        let logger = Logger.log4net
        let mutable shutDownInfo : WrkNodeWcfSvcShutDownInfo option = None

        let tryDispose() =
            match shutDownInfo with
            | Some i ->
                cleanupService logger i
                shutDownInfo <- None
            | None -> ignore()

        override _.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = workerNodeServiceProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo results
            shutDownInfo <- startWrkNodeWcfServiceRun logger i

        override _.OnStop () =
            tryDispose()
            base.OnStop()

        interface IDisposable with
            member _.Dispose() = tryDispose()
