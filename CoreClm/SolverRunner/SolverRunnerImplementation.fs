namespace SolverRunner

open Argu
open Clm.ModelParams
open ClmSys.ClmErrors
open ClmSys.ExitErrorCodes
open ClmSys.GeneralPrimitives
open ServiceProxy.SolverProcessProxy
open ServiceProxy.WorkerNodeProxy
open SolverRunner.SolverRunnerCommandLine
open System
open Argu

open Softellect.Sys.Core
open Softellect.Sys
open Softellect.Messaging.ServiceInfo
open Softellect.Wcf.Common
open Softellect.Messaging.Primitives
open Softellect.Messaging.Client
open Softellect.Messaging.Proxy

open ClmSys.Logging
open ClmSys.WorkerNodeData
open ClmSys.TimerEvents
open ContGenServiceInfo.ServiceInfo
open WorkerNodeServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
open MessagingServiceInfo.ServiceInfo
open Clm.ModelParams
open ServiceProxy.WorkerNodeProxy
open ServiceProxy.MsgProcessorProxy
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.WorkerNodeErrors
open ClmSys.WorkerNodePrimitives
open ServiceProxy.SolverRunner
open SolverRunner.SolverRunnerTasks
open ClmSys.SolverRunnerPrimitives
open ClmSys.MessagingData
open ClmSys.SolverRunnerErrors


module SolverRunnerImplementation =

    let private toError g f = f |> g |> SolverRunnerErr |> Error
    let private addError g f e = ((f |> g |> SolverRunnerErr) + e) |> Error


    let onSaveResult (proxy : SendMessageProxy) (r : ResultDataWithId) =
        printfn "onSaveResult: Sending results with resultDataId = %A." r.resultDataId

        {
            partitionerRecipient = proxy.partitionerId
            deliveryType = GuaranteedDelivery
            messageData = r |> SaveResultPrtMsg
        }.getMessageInfo()
        |> proxy.sendMessage
        |> Rop.bindError (addError OnSaveResultErr (SendResultMessageError (proxy.partitionerId.messagingClientId, r.resultDataId)))


    let onSaveCharts (proxy : SendMessageProxy) (r : ChartGenerationResult) =
        match r with
        | GeneratedCharts c ->
            printfn "onSaveCharts: Sending charts with resultDataId = %A." c.resultDataId

            {
                partitionerRecipient = proxy.partitionerId
                deliveryType = GuaranteedDelivery
                messageData = c |> SaveChartsPrtMsg
            }.getMessageInfo()
            |> proxy.sendMessage
            |> Rop.bindError (addError OnSaveChartsErr (SendChartMessageError (proxy.partitionerId.messagingClientId, c.resultDataId)))
        | NotGeneratedCharts ->
            printfn "onSaveCharts: No charts."
            Ok()


    let toDeliveryType progress =
        match progress with
        | NotStarted -> (GuaranteedDelivery, false)
        | InProgress _ -> (NonGuaranteedDelivery, false)
        | Completed _ -> (GuaranteedDelivery, true)
        | Failed _ -> (GuaranteedDelivery, true)
        | Cancelled _ -> (GuaranteedDelivery, true)
        | AllCoresBusy _ -> (GuaranteedDelivery, true)


    let onUpdateProgress (proxy : OnUpdateProgressProxy) (p : ProgressUpdateInfo) =
        //printfn "onUpdateProgress: runQueueId = %A, progress = %A." p.runQueueId p.progress
        let t, completed = toDeliveryType p.progress

        let r1 =
            {
                partitionerRecipient = proxy.sendMessageProxy.partitionerId
                deliveryType = t
                messageData = UpdateProgressPrtMsg p
            }.getMessageInfo()
            |> proxy.sendMessageProxy.sendMessage
            |> Rop.bindError (addError OnUpdateProgressErr (UnableToSendProgressMsgErr p.runQueueId))

        if completed
        then
            let r2 = proxy.tryDeleteWorkerNodeRunModelData p.runQueueId
            combineUnitResults r1 r2
        else r1


    type SolverProcess(proxy : SolverProcessProxy) =

        member x.run() : unit =
            failwith "SolverProcess.run is not yet implemented."


    let tryCreateSolver q : ClmResult<SolverProcess> =
        let proxy = SolverProcessProxy.create q
        failwith "tryCreateSolver is not yet implemented."


    let runSolver (results : ParseResults<SolverRunnerArguments>) usage : int =
        match results.TryGetResult RunQueue with
        | Some q ->
            match RunQueueId q |> tryCreateSolver with
            | Ok solver ->
                solver.run()
                CompletedSuccessfully
            | Error e ->
                printfn $"runSolver: Error: {e}."
                DatabaseErrorOccurred
        | None ->
            printfn $"runSolver: {usage}."
            InvalidCommandLineArgs

