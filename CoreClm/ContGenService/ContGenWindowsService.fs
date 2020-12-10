namespace ContGenService

open System
open System.ServiceProcess
open System.ServiceModel
open Argu
open ContGenServiceInfo.ServiceInfo
open ClmSys.Logging
open ContGenService.SvcCommandLine
open ClmSys.ContGenPrimitives
open ClmSys.ContGenData
open ClmSys.Wcf
open ContGen.ModelRunner
open ClmSys.ContGenErrors
open ClmSys.ClmErrors
open ClmSys

module WindowsService =

    let mutable serviceData = getContGenServiceData logger []

    let tryCreateModelRunner() =
        match serviceData with
        | Ok data -> ModelRunner.create data.modelRunnerData
        | Error e -> Error e

    let private modelRunner = new Lazy<ClmResult<ModelRunner>>(tryCreateModelRunner)


    [<ServiceBehavior(IncludeExceptionDetailInFaults = true, InstanceContextMode = InstanceContextMode.Single)>]
    type ContGenWcfService() =
        let toCancelRunQueueError f = f |> TryCancelRunQueueWcfErr |> TryCancelRunQueueErr |> ContGenServiceErr
        let toRequestResultsError f = f |> TryRequestResultsWcfErr |> TryRequestResultsErr |> ContGenServiceErr
        let tryCancelRunQueue (q, c) = modelRunner.Value |> Rop.bind (fun e -> e.tryCancelRunQueue q c)
        let tryRequestResults (q, c) = modelRunner.Value |> Rop.bind (fun e -> e.tryRequestResults q c)

        interface IContGenWcfService with
            member _.tryCancelRunQueue b = tryReply tryCancelRunQueue toCancelRunQueueError b
            member _.tryRequestResults b = tryReply tryRequestResults toRequestResultsError b


    let startContGenWcfServiceRun (logger : Logger) (j : ClmResult<ContGenServiceData>) : ContGenWcfSvcShutDownInfo option =
        try
            printfn "startContGenWcfServiceRun: Creating WCF ContGen Service..."
            serviceData <- j

            match modelRunner.Value, j  with
            | Ok r, Ok i ->
                r.start()

                let binding = getBinding()
                let baseAddress = new Uri(i.contGenServiceAccessInfo.wcfServiceUrl)
                let serviceHost = new ServiceHost(typeof<ContGenWcfService>, baseAddress)
                let d = serviceHost.AddServiceEndpoint(typeof<IContGenWcfService>, binding, baseAddress)
                do serviceHost.Open()
                printfn "startContGenWcfServiceRun: Completed."

                {
                    contGenServiceHost = serviceHost
                }
                |> Some
            | Error e, Ok _ ->
                printfn "startContGenWcfServiceRun: Error - %A." e
                None
            | Ok _, Error e ->
                printfn "startContGenWcfServiceRun: Error - %A." e
                None
            | Error e1, Error e2 ->
                printfn "startContGenWcfServiceRun: Errors - %A, %A." e1 e2
                None
        with
        | e ->
            logger.logExn "startContGenWcfServiceRun: Error starting WCF ContGen Service." e
            None


    let cleanupService (logger : Logger) (i : ContGenWcfSvcShutDownInfo) =
        try
            logger.logInfoString "ContGenWindowsService: Closing WCF service host."
            i.contGenServiceHost.Close()
        with
        | e -> logger.logExn "ContGenWindowsService: Exception occurred: " e


    type public ContGenWindowsService () =
        inherit ServiceBase (ServiceName = contGenServiceName.value.value)

        let logger = Logger.log4net
        let initService () = ()
        do initService ()
        let mutable shutDownWcfInfo : ContGenWcfSvcShutDownInfo option = None

        let tryDispose() =
            match shutDownWcfInfo with
            | Some i ->
                cleanupService logger i
                shutDownWcfInfo <- None
            | None -> ignore()

        override _.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<ContGenRunArgs>(programName = contGenServiceProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getContGenServiceData logger results
            shutDownWcfInfo <- startContGenWcfServiceRun logger i

        override _.OnStop () =
            tryDispose()
            base.OnStop()

        interface IDisposable with
            member _.Dispose() = tryDispose()
