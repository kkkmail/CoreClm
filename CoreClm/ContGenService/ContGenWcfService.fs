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
        | Ok data ->
            match ModelRunner.create None data.modelRunnerData with
            | Ok v -> Ok v
            | Error e -> TryCreateModelRunnerErr e |> ContGenServiceErr |> Error
        | Error e -> Error e


    let private modelRunner = new Lazy<ClmResult<ModelRunner>>(tryCreateModelRunner)


    let tryStartModelRunner() =
        match modelRunner.Value with
        | Ok service -> service.start() |> Ok
        | Error e -> Error e


    type ContGenWcfService() =
        let toCancelRunQueueError f = f |> TryCancelRunQueueWcfErr |> ContGenServiceErr
        let toRequestResultsError f = f |> TryRequestResultsWcfErr |> ContGenServiceErr
        let toResetError f = f |> TryResetWcfErr |> ContGenServiceErr
        let tryCancelRunQueue (q, c) = modelRunner.Value |> Rop.bind (fun e -> e.tryCancelRunQueue q c)
        let tryRequestResults (q, c) = modelRunner.Value |> Rop.bind (fun e -> e.tryRequestResults q c)
        let tryReset q = modelRunner.Value |> Rop.bind (fun e -> e.tryReset q)

        interface IContGenWcfService with
            member _.tryCancelRunQueue b = tryReply tryCancelRunQueue toCancelRunQueueError b
            member _.tryRequestResults b = tryReply tryRequestResults toRequestResultsError b
            member _.tryReset b = tryReply tryReset toResetError b


    type ContGenWcfServiceImpl = WcfService<ContGenWcfService, IContGenWcfService, ContGenServiceData>
