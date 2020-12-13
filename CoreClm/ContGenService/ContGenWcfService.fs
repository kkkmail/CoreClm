namespace ContGenService

open System

open Softellect.Sys
open Softellect.Wcf.Service
open ClmSys.Logging
open ClmSys.ClmErrors
open ClmSys.ContGenErrors
open ContGen.ModelRunner
open ContGenServiceInfo.ServiceInfo
open ContGenService.SvcCommandLine

module ContGenWcfService =

    let contGenServiceData = Lazy<Result<ContGenServiceData, ClmError>>(fun () -> tryGetContGenServiceData (Logger.defaultValue) [])


    let private tryCreateModelRunner() =
        match contGenServiceData.Value with
        | Ok data -> ModelRunner.create data.modelRunnerData
        | Error e -> Error e


    let private modelRunner = new Lazy<ClmResult<ModelRunner>>(tryCreateModelRunner)

    //type ContGenServiceData =
    //    {
    //        modelRunnerData : ModelRunnerDataWithProxy
    //        contGenServiceAccessInfo : ContGenServiceAccessInfo
    //    }


    ////type MessagingClient = MessagingClient<ClmMessageData, ClmError>
    ////type MessagingClientData = MessagingClientData<ClmMessageData, ClmError>
    ////type MessagingServiceData = MessagingServiceData<ClmMessageData, ClmError>
    type ContGenWcfServiceData = WcfServiceData<ContGenServiceData>
    //type Message = Message<ClmMessageData>
    //type MessageInfo = MessageInfo<ClmMessageData>
    //type MessagingService = MessagingService<ClmMessageData, ClmError>
    //type MessagingWcfService = MessagingWcfService<ClmMessageData, ClmError>
    //type MessagingWcfServiceImpl = WcfService<MessagingWcfService, IMessagingWcfService, MessagingServiceData>


    //type ContGenService() =

    //    interface IContGenService with
    //        member _.tryCancelRunQueue r c = failwith "123456"
    //        member _.tryRequestResults r n = failwith "123456"

    //    //abstract tryCancelRunQueue : RunQueueId -> CancellationType -> UnitResult
    //    //abstract tryRequestResults : RunQueueId -> ResultNotificationType -> UnitResult


    type ContGenWcfService() =
        let toCancelRunQueueError f = f |> TryCancelRunQueueWcfErr |> TryCancelRunQueueErr |> ContGenServiceErr
        let toRequestResultsError f = f |> TryRequestResultsWcfErr |> TryRequestResultsErr |> ContGenServiceErr
        let tryCancelRunQueue (q, c) = modelRunner.Value |> Rop.bind (fun e -> e.tryCancelRunQueue q c)
        let tryRequestResults (q, c) = modelRunner.Value |> Rop.bind (fun e -> e.tryRequestResults q c)

        interface IContGenWcfService with
            member _.tryCancelRunQueue b = tryReply tryCancelRunQueue toCancelRunQueueError b
            member _.tryRequestResults b = tryReply tryRequestResults toRequestResultsError b


    type ContGenWcfServiceImpl = WcfService<ContGenWcfService, IContGenWcfService, ContGenServiceData>


    let tryGetServiceData serviceAccessInfo wcfLogger serviceData =
        match WcfServiceAccessInfo.tryCreate serviceAccessInfo  with
        | Ok i ->
            {
                wcfServiceAccessInfo = i

                wcfServiceProxy =
                    {
                        wcfLogger = wcfLogger
                    }

                serviceData = serviceData
                setData = fun _ -> ignore()
            }
            |> Ok
        | Error e -> Error e
