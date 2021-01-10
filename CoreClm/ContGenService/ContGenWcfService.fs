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

    type ContGenWcfServiceData = WcfServiceData<ContGenServiceData>

    let contGenServiceData = Lazy<Result<ContGenServiceData, ClmError>>(fun () -> tryGetContGenServiceData (Logger.defaultValue) [])


    let private tryCreateModelRunner() =
        match contGenServiceData.Value with
        | Ok data -> ModelRunner.create None data.modelRunnerData
        | Error e -> Error e


    let private modelRunner = new Lazy<ClmResult<ModelRunner>>(tryCreateModelRunner)


    let tryStartModelRunner() =
        match modelRunner.Value with
        | Ok service -> service.start() |> Ok
        | Error e -> Error e


    type ContGenWcfService() =
        let toCancelRunQueueError f = f |> TryCancelRunQueueWcfErr |> TryCancelRunQueueErr |> ContGenServiceErr
        let toRequestResultsError f = f |> TryRequestResultsWcfErr |> TryRequestResultsErr |> ContGenServiceErr
        let tryCancelRunQueue (q, c) = modelRunner.Value |> Rop.bind (fun e -> e.tryCancelRunQueue q c)
        let tryRequestResults (q, c) = modelRunner.Value |> Rop.bind (fun e -> e.tryRequestResults q c)

        interface IContGenWcfService with
            member _.tryCancelRunQueue b = tryReply tryCancelRunQueue toCancelRunQueueError b
            member _.tryRequestResults b = tryReply tryRequestResults toRequestResultsError b


    type ContGenWcfServiceImpl = WcfService<ContGenWcfService, IContGenWcfService, ContGenServiceData>
