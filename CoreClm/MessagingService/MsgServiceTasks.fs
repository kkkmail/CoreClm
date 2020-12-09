namespace MessagingService

open Argu
//open Softellect.Sys.ServiceInstaller
open Softellect.Sys.Logging
open Softellect.Messaging.Primitives
open ClmSys.MessagingData
//open ClmSys.Logging
open ClmSys.MessagingPrimitives
open ClmSys.ClmWorker
open MessagingService.SvcCommandLine
//open MessagingService.MsgWindowsService
open MessagingServiceInfo.ServiceInfo

module ServiceTasks =

    type MessagingConfigParam
        with
        static member fromParseResults (p : ParseResults<MessagingServiceRunArgs>) : list<MessagingConfigParam> =
            [
            ]
            |> List.choose id


    //let runService l (_ : list<MessagingConfigParam>, i) = tryStartMsgWcfServiceRun l i


    //let serviceInfo : ServiceInfo<(list<MessagingConfigParam> * MsgSettings), MsgWcfSvcShutDownInfo> =
    //    {
    //        serviceName = messagingServiceName.value
    //        runService = runService
    //        cleanup = cleanupService
    //        timeoutMilliseconds = None
    //        logger = Logger.defaultValue
    //    }


    let getParams p = MessagingConfigParam.fromParseResults p, getServiceSettings (p.GetAllResults())
    let getSaveSettings (p : ParseResults<MessagingServiceRunArgs>) () = p.GetAllResults() |> saveSettings
    type MessagingServiceTask = WorkerTask<(list<MessagingConfigParam> * MsgSettings), MessagingServiceRunArgs>
