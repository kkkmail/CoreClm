namespace MessagingService

open System
open System.ServiceProcess
open Argu

open Softellect.Sys.Logging
open Softellect.Sys.MessagingPrimitives
open Softellect.Messaging.Primitives
open Softellect.Messaging.Proxy
open Softellect.Messaging.ServiceInfo
open Softellect.Messaging.Service

open ClmSys.MessagingData
//open ClmSys.Logging
open MessagingServiceInfo.ServiceInfo
open MessagingService.ServiceImplementation
open MessagingService.SvcCommandLine
open System.ServiceModel
open ClmSys.MessagingPrimitives
open ClmSys.ClmErrors
open ClmSys.VersionInfo

module MsgWindowsService =

    let startMsgWcfServiceRun logger (i : MessagingServiceInfo) : MsgWcfSvcShutDownInfo option =
        //try
        //    printfn "startMsgWcfServiceRun: Creating WCF Messaging Service..."
        //    serviceAccessInfo <- i
        //    let binding = getBinding()
        //    let baseAddress = Uri(i.messagingSvcInfo.wcfServiceUrl)

        //    let serviceHost = new ServiceHost(typeof<MessagingWcfService>, baseAddress)

        //    let d = serviceHost.AddServiceEndpoint(typeof<IMessagingWcfService>, binding, baseAddress)
        //    do serviceHost.Open()
        //    printfn "startMsgWcfServiceRun: ... completed."

        //    {
        //        serviceHost = serviceHost
        //    }
        //    |> Some
        //with
        //| e ->
        //    logger.logExn "startMsgWcfServiceRun: Error starting WCF Messaging Service." e
        //    None

        let serviceProxy = 0
        let msgServiceAccessInfo = i.messagingSvcInfo.messagingServiceAccessInfo
        //let serviceData : MessagingServiceData<ClmMessageData, ClmError> = 0

        let serviceData : MessagingServiceData<ClmMessageData, ClmError> =
            {
                messagingServiceInfo =
                    {
                        expirationTime = i.messagingInfo.expirationTime
                        messagingDataVersion = messagingDataVersion
                    }

                communicationType = i.communicationType
                messagingServiceProxy = serviceProxy
            }

        let msgServiceDataRes = tryGetMsgServiceData msgServiceAccessInfo logger serviceData

        msgServiceDataRes


    let cleanupService (logger : Logger) (i : MsgWcfSvcShutDownInfo) =
        try
            logger.logInfoString "MessagingWindowsService: Closing WCF service host."
            i.serviceHost.Close()
        with
        | e -> logger.logExn "MessagingWindowsService: Exception occurred: " e


    type public MessagingWindowsService () =
        inherit ServiceBase (ServiceName = messagingServiceName.value.value)

        let initService () = ()
        do initService ()
        let logger = Logger.log4net
        let mutable shutDownWcfInfo : MsgWcfSvcShutDownInfo option = None

        let tryDispose() =
            match shutDownWcfInfo with
            | Some i ->
                cleanupService logger i
                shutDownWcfInfo <- None
            | None -> ignore()

        override _.OnStart (args : string[]) =
            base.OnStart(args)
            let parser = ArgumentParser.Create<MessagingServiceRunArgs>(programName = messagingProgramName)
            let results = (parser.Parse args).GetAllResults()
            let i = getServiceAccessInfo results
            shutDownWcfInfo <- startMsgWcfServiceRun logger i

        override _.OnStop () =
            tryDispose()
            base.OnStop()

        interface IDisposable with
            member _.Dispose() = tryDispose()
