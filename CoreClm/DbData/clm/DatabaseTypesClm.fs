#nowarn "1104"

namespace DbData

open Newtonsoft.Json
open FSharp.Data.Sql
open System

//open Primitives.GeneralPrimitives
open Softellect.Sys
open Softellect.Sys.Core
open Softellect.Messaging.Primitives
open Softellect.Sys.Rop
open Softellect.Sys.DataAccess

open Clm.Substances
open Clm.ModelParams
open Clm.CalculationData
open Clm.ReactionRates
open ClmSys.GeneralErrors
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
//open ClmSys.GeneralPrimitives
//open ClmSys.WorkerNodePrimitives
//open ClmSys.WorkerNodeData
//open ClmSys.PartitionerData
open DbData.DatabaseTypesDbo

// ! Must be the last to open !
open DbData.Configuration

module DatabaseTypesClm =

    type private ClmDefaultValueEntity = ClmContext.``clm.DefaultValueEntity``
    type private ClmTaskEntity = ClmContext.``clm.TaskEntity``
    type private CommandLineParamEntity = ClmContext.``clm.CommandLineParamEntity``
    type private ModelDataEntity = ClmContext.``clm.ModelDataEntity``
    type private ClmRunQueueEntity = ClmContext.``clm.RunQueueEntity``


    type private RunQueueTableData =
        {
            runQueue : RunQueueEntity
            clmRunQueue : ClmRunQueueEntity
            clmDefaultValueId : int64
        }


    let private createClmDefaultValue (r : ClmDefaultValueEntity) =
        {
            clmDefaultValueId = r.DefaultValueId |> ClmDefaultValueId
            defaultRateParams = r.DefaultRateParams |> JsonConvert.DeserializeObject<ReactionRateProviderParams>
            description = r.Description
        }


    let private mapOption f e =
        match e with
        | Some v -> Ok v
        | None -> f()


    let loadClmDefaultValue c (clmDefaultValueId : ClmDefaultValueId) =
        let elevate e = e |> LoadClmDefaultValueErr
        let toError e = e |> elevate |> Error
        let fromDbError e = e |> LoadClmDefaultValueDbErr |> elevate

        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for c in ctx.Clm.DefaultValue do
                    where (c.DefaultValueId = clmDefaultValueId.value)
                    select (Some c)
                    exactlyOneOrDefault
                }

            x
            |> Option.map createClmDefaultValue
            |> mapOption (fun () -> (CannotLoadClmDefaultValue clmDefaultValueId |> toError))

        tryDbFun fromDbError g


    let upsertClmDefaultValue c (p : ClmDefaultValue) =
        let elevate e = e |> UpsertClmDefaultValueErr
        //let toError e = e |> elevate |> Error
        let fromDbError e = e |> UpsertClmDefaultValueDbErr |> elevate

        let g() =
            let ctx = getDbContext c

            let r = ctx.Procedures.ClmUpsertDefaultValue.Invoke(
                            ``@defaultValueId`` = p.clmDefaultValueId.value,
                            ``@defaultRateParams`` = (p.defaultRateParams |> JsonConvert.SerializeObject),
                            ``@description`` = (match p.description with | Some d -> d | None -> null))

            let x = bindIntScalar (fun a -> CannotUpsertClmDefaultValue a |> elevate) (p.clmDefaultValueId) r.ResultSet
            x

        tryDbFun fromDbError g


    let private createModelCommandLineParam (r : CommandLineParamEntity) =
        {
            y0 = r.Y0
            tEnd = r.TEnd
            useAbundant = r.UseAbundant
        }


    let loadCommandLineParams c (ClmTaskId clmTaskId) =
        let elevate e = e |> LoadCommandLineParamsErr
        //let toError e = e |> elevate |> Error
        let fromDbError e = e |> LoadCommandLineParamsDbErr |> elevate

        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for p in ctx.Clm.CommandLineParam do
                    where (p.TaskId = clmTaskId)
                    sortBy p.CreatedOn
                    select p
                }

            x
            |> Seq.toList
            |> List.map createModelCommandLineParam
            |> Ok

        tryDbFun fromDbError g


    let private addCommandLineParamRow (ctx : ClmContext) (ClmTaskId clmTaskId) (p : ModelCommandLineParam) =
        let row = ctx.Clm.CommandLineParam.Create(
                            TaskId = clmTaskId,
                            CommandLineParamId = Guid.NewGuid(),
                            Y0 = p.y0,
                            TEnd = p.tEnd,
                            UseAbundant = p.useAbundant)

        row


    let addCommandLineParams c clmTaskId (p : ModelCommandLineParam) =
        let elevate e = e |> AddCommandLineParamsErr
        //let toError e = e |> elevate |> Error
        let fromDbError e = e |> AddCommandLineParamsDbErr |> elevate

        let g() =
            let ctx = getDbContext c
            let row = addCommandLineParamRow ctx clmTaskId p
            ctx.SubmitUpdates()
            Ok()

        tryDbFun fromDbError g


    let private tryCreateClmTask c (r : ClmTaskEntity) =
        let elevate e = e |> TryCreateClmTaskErr
        let toError e = e |> elevate |> Error
        //let fromDbError e = e |> TryCreateClmTaskDbErr |> elevate

        let clmTaskId = r.TaskId |> ClmTaskId

        match r.NumberOfAminoAcids |> NumberOfAminoAcids.tryCreate, r.MaxPeptideLength |> MaxPeptideLength.tryCreate with
        | Some n, Some m ->

            match c clmTaskId with
            | Ok p ->
                {
                    clmTaskInfo =
                        {
                            clmTaskId = clmTaskId

                            taskDetails =
                                {
                                    clmDefaultValueId = r.DefaultValueId |> ClmDefaultValueId
                                    clmTaskPriority = r.TaskPriority |> ClmTaskPriority
                                    numberOfAminoAcids = n
                                    maxPeptideLength = m
                                }
                        }
                    commandLineParams = p
                    numberOfRepetitions = r.NumberOfRepetitions
                    remainingRepetitions = r.RemainingRepetitions
                    createdOn = r.CreatedOn
                }
                |> Ok
            | Error e -> addError (ErrorWhenCreatingClmTask clmTaskId |> toError) e
        | _ -> toError (CannotCreateClmTask clmTaskId)


    let loadClmTask c (clmTaskId : ClmTaskId) =
        let elevate e = e |> LoadClmTaskErr
        let toError e = e |> elevate |> Error
        let fromDbError e = e |> LoadClmTaskDbErr |> elevate

        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for c in ctx.Clm.Task do
                    where (c.TaskId = clmTaskId.value)
                    select (Some c)
                    exactlyOneOrDefault
                }

            match x with
            | Some v -> tryCreateClmTask (loadCommandLineParams c) v
            | None -> toError (CannotLoadClmTask clmTaskId)

        tryDbFun fromDbError g


    let loadIncompleteClmTasks c =
        let elevate e = e |> LoadIncompleteClmTasksErr
        //let toError e = e |> elevate |> Error
        let fromDbError e = e |> LoadIncompleteClmTasksDbErr |> elevate

        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for c in ctx.Clm.Task do
                    where (c.RemainingRepetitions > 0 && c.TaskStatusId = 0)
                    select c
                }

            x
            |> Seq.toList
            |> List.map (fun r -> tryCreateClmTask (loadCommandLineParams c) r)
            |> Ok

        tryDbFun fromDbError g


    let private addClmTaskRow  (ctx : ClmContext) (r : ClmTask) =
        let row = ctx.Clm.Task.Create(
                            TaskId = r.clmTaskInfo.clmTaskId.value,
                            DefaultValueId = r.clmTaskInfo.taskDetails.clmDefaultValueId.value,
                            TaskPriority = r.clmTaskInfo.taskDetails.clmTaskPriority.value,
                            NumberOfAminoAcids = r.clmTaskInfo.taskDetails.numberOfAminoAcids.length,
                            MaxPeptideLength = r.clmTaskInfo.taskDetails.maxPeptideLength.length,
                            NumberOfRepetitions = r.numberOfRepetitions,
                            RemainingRepetitions = r.remainingRepetitions,
                            CreatedOn = DateTime.Now)

        row


    let addClmTask c (clmTask : ClmTask) =
        let elevate e = e |> AddClmTaskErr
        //let toError e = e |> elevate |> Error
        let fromDbError e = e |> AddClmTaskDbErr |> elevate

        let g() =
            let ctx = getDbContext c
            let row = addClmTaskRow ctx clmTask
            ctx.SubmitUpdates()

            let result =
                clmTask.commandLineParams
                |> List.map (addCommandLineParams c clmTask.clmTaskInfo.clmTaskId)
                |> foldUnitResults

            result

        tryDbFun fromDbError g


    /// Updates remainingRepetitions of ClmTask.
    let updateClmTask c (clmTask : ClmTask) =
        let elevate e = e |> UpdateClmTaskErr
        let toError e = e |> elevate |> Error
        let fromDbError e = e |> UpdateClmTaskDbErr |> elevate

        let g() =
            let ctx = getDbContext c

            let r = ctx.Procedures.ClmUpdateTask.Invoke(
                            ``@taskId`` = clmTask.clmTaskInfo.clmTaskId.value,
                            ``@remainingRepetitions`` = clmTask.remainingRepetitions)

            let m = r.ResultSet |> mapIntScalar

            match m with
            | Some 1 -> Ok ()
            | _ -> toError (CannotUpdateClmTask clmTask.clmTaskInfo.clmTaskId)

        tryDbFun fromDbError g


    let private tryCreateModelData (c : ClmTaskId -> ClmResult<ClmTask>) (r : ModelDataEntity) =
        let elevate e = e |> TryCreateModelDataErr
        let toError e = e |> elevate |> Error
        //let fromDbError e = e |> TryCreateModelDataDbErr |> elevate

        let modelDataId = r.ModelDataId |> ModelDataId

        match r.TaskId |> ClmTaskId |> c with
        | Ok i ->
            let rawData =
                {
                    seedValue = r.SeedValue
                    modelData =
                        {
                            modelDataParams = r.ModelDataParams |> JsonConvert.DeserializeObject<ModelDataParams>
                            modelBinaryData = r.ModelBinaryData |> unZip |> JsonConvert.DeserializeObject<ModelBinaryData>
                        }
                }

            {
                modelDataId = modelDataId
                clmTaskInfo = i.clmTaskInfo
                data = rawData
            }
            |> Ok
        | Error e ->  addError (ErrorWhenCreatingModelData modelDataId |> toError) e


    let loadModelData c (modelDataId : ModelDataId) =
        let elevate e = e |> LoadModelDataErr
        let toError e = e |> elevate |> Error
        let fromDbError e = e |> LoadModelDataDbErr |> elevate

        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for m in ctx.Clm.ModelData do
                    where (m.ModelDataId = modelDataId.value)
                    select (Some m)
                    exactlyOneOrDefault
                }

            match x with
            | Some v -> tryCreateModelData (loadClmTask c) v
            | None -> toError (CannotLoadModelData modelDataId)

        tryDbFun fromDbError g


    //let upsertModelData c (m : ModelData) =
    //    let elevate e = e |> UpsertModelDataErr
    //    let toError e = e |> elevate |> Error
    //    let fromDbError e = e |> UpsertModelDataDbErr |> elevate

    //    let g() =
    //        let ctx = getDbContext c

    //        let r = ctx.Procedures.ClmUpsertModelData.Invoke(
    //                    ``@modelDataId`` = m.modelDataId.value,
    //                    ``@taskId`` = m.clmTaskInfo.clmTaskId.value,
    //                    ``@seedValue`` = (match m.data.seedValue with | Some s -> s | None -> -1),
    //                    ``@modelDataParams`` = (m.data.modelData.modelDataParams |> JsonConvert.SerializeObject),
    //                    ``@modelBinaryData`` = (m.data.modelData.modelBinaryData |> JsonConvert.SerializeObject |> zip),
    //                    ``@createdOn`` = DateTime.Now)


    //        let x = r.ResultSet |> mapIntScalar

    //        match x with
    //        | Some 1 -> Ok ()
    //        | _ -> toError (CannotUpsertModelData m.modelDataId)

    //    tryDbFun fromDbError g


    //let private mapRunQueue (r: RunQueueTableData) =
    //    let elevate e = e |> MapRunQueueErr
    //    let toError e = e |> elevate |> Error
    //    //let fromDbError e = e |> MapRunQueueDbErr |> elevate
    //
    //    let runQueueId = RunQueueId r.runQueue.RunQueueId
    //
    //    match RunQueueStatus.tryCreate r.runQueue.RunQueueStatusId with
    //    | Some s ->
    //        {
    //            runQueueId = runQueueId
    //
    //            info =
    //                {
    //                    modelDataId = ModelDataId r.clmRunQueue.ModelDataId
    //                    defaultValueId = ClmDefaultValueId r.clmDefaultValueId
    //
    //                    modelCommandLineParam =
    //                        {
    //                            y0 = r.clmRunQueue.Y0
    //                            tEnd = r.clmRunQueue.TEnd
    //                            useAbundant = r.clmRunQueue.UseAbundant
    //                        }
    //                }
    //
    //            runQueueStatus = s
    //            workerNodeIdOpt = r.runQueue.WorkerNodeId |> Option.bind (fun e -> e |> MessagingClientId |> WorkerNodeId |> Some)
    //
    //            progressData =
    //                {
    //                    progressData =
    //                        {
    //                            progress = r.runQueue.Progress
    //                            callCount = r.runQueue.CallCount
    //                            errorMessageOpt = r.runQueue.ErrorMessage |> Option.map ErrorMessage
    //                        }
    //                    yRelative = r.runQueue.RelativeInvariant
    //
    //                    eeData =
    //                        {
    //                            maxEe = r.clmRunQueue.MaxEe
    //                            maxAverageEe = r.clmRunQueue.MaxAverageEe
    //                            maxWeightedAverageAbsEe = r.clmRunQueue.MaxWeightedAverageAbsEe
    //                            maxLastEe = r.clmRunQueue.MaxLastEe
    //                        }
    //                }
    //
    //            createdOn = r.runQueue.CreatedOn
    //        }
    //        |> Ok
    //    | None -> toError (CannotMapRunQueue runQueueId)


    //let private mapRunQueueResults x =
    //    x
    //    |> List.ofSeq
    //    |> List.map (fun (a, b, c) -> { runQueue = a; clmRunQueue = b; clmDefaultValueId = c })
    //    |> List.map mapRunQueue


    ///// Loads all not started RunQueue.
    //let loadRunQueue c =
    //    let elevate e = e |> LoadRunQueueErr
    //    //let toError e = e |> elevate |> Error
    //    let fromDbError e = e |> LoadRunQueueDbErr |> elevate
    //
    //    let g() =
    //        let ctx = getDbContext c
    //
    //        let x =
    //            query {
    //                for r in ctx.Dbo.RunQueue do
    //                join cr in ctx.Clm.RunQueue on (r.RunQueueId = cr.RunQueueId)
    //                join m in ctx.Clm.ModelData on (cr.ModelDataId = m.ModelDataId)
    //                join t in ctx.Clm.Task on (m.TaskId = t.TaskId)
    //                where (r.RunQueueStatusId = 0 && r.Progress = 0.0m && t.TaskStatusId = 0 && r.WorkerNodeId = None)
    //                select (r, cr, t.DefaultValueId)
    //            }
    //
    //        mapRunQueueResults x |> Ok
    //
    //    tryDbFun fromDbError g


    ///// Loads all currently running models == total progress.
    ///// RunQueueStatusId = 2 is InProgressRunQueue.
    //let loadRunQueueProgress c =
    //    let elevate e = e |> LoadRunQueueProgressErr
    //    //let toError e = e |> elevate |> Error
    //    let fromDbError e = e |> LoadRunQueueProgressDbErr |> elevate
    //
    //    let g() =
    //        let ctx = getDbContext c
    //
    //        let x =
    //            query {
    //                for r in ctx.Dbo.RunQueue do
    //                join cr in ctx.Clm.RunQueue on (r.RunQueueId = cr.RunQueueId)
    //                join m in ctx.Clm.ModelData on (cr.ModelDataId = m.ModelDataId)
    //                join t in ctx.Clm.Task on (m.TaskId = t.TaskId)
    //                where (r.RunQueueStatusId = 2 && t.TaskStatusId = 0 && r.WorkerNodeId <> None)
    //                sortBy t.TaskPriority
    //                thenBy r.RunQueueOrder
    //                select (r, cr, t.DefaultValueId)
    //            }
    //
    //        mapRunQueueResults x |> Ok
    //
    //    tryDbFun fromDbError g


    ///// Loads first not started RunQueue.
    //let tryLoadFirstRunQueue c =
    //    let elevate e = e |> TryLoadFirstRunQueueErr
    //    //let toError e = e |> elevate |> Error
    //    let fromDbError e = e |> TryLoadFirstRunQueueDbErr |> elevate

    //    let g() =
    //        let ctx = getDbContext c

    //        let x =
    //            query {
    //                for r in ctx.Dbo.RunQueue do
    //                join cr in ctx.Clm.RunQueue on (r.RunQueueId = cr.RunQueueId)
    //                join m in ctx.Clm.ModelData on (cr.ModelDataId = m.ModelDataId)
    //                join t in ctx.Clm.Task on (m.TaskId = t.TaskId)
    //                where (r.RunQueueStatusId = 0 && r.Progress = 0.0m && t.TaskStatusId = 0 && r.WorkerNodeId = None)
    //                sortBy t.TaskPriority
    //                thenBy r.RunQueueOrder
    //                select (r, cr, t.DefaultValueId)
    //            }

    //        mapRunQueueResults x |> List.tryHead |> Ok

    //    tryDbFun fromDbError g |> Rop.unwrapResultOption


    ///// Loads RunQueue by runQueueId.
    //let tryLoadRunQueue c (q : RunQueueId) =
    //    let elevate e = e |> TryLoadRunQueueErr
    //    //let toError e = e |> elevate |> Error
    //    let fromDbError e = e |> TryLoadRunQueueDbErr |> elevate

    //    let g() =
    //        let ctx = getDbContext c

    //        let x =
    //            query {
    //                for r in ctx.Dbo.RunQueue do
    //                join cr in ctx.Clm.RunQueue on (r.RunQueueId = cr.RunQueueId)
    //                join m in ctx.Clm.ModelData on (cr.ModelDataId = m.ModelDataId)
    //                join t in ctx.Clm.Task on (m.TaskId = t.TaskId)
    //                where (r.RunQueueId = q.value)
    //                select (r, cr, t.DefaultValueId)
    //            }

    //        mapRunQueueResults x |> List.tryHead |> Ok

    //    tryDbFun fromDbError g |> Rop.unwrapResultOption


    ///// Tries to reset RunQueue.
    //let tryResetRunQueue c (q : RunQueueId) =
    //    let g() =
    //        let ctx = getDbContext c
    //        let r = ctx.Procedures.TryResetRunQueue.Invoke q.value
    //        let m = r.ResultSet |> mapIntScalar

    //        match m with
    //        | Some 1 -> Ok ()
    //        | _ -> toError ResetRunQueueEntryErr q

    //    tryDbFun g


    //let private addRunQueueRow (ctx : ClmContext) (r : RunQueue) =
    //    let row = ctx.Dbo.RunQueue.Create(
    //                        RunQueueId = r.runQueueId.value,
    //                        ModelTypeId = 1,
    //                        WorkerNodeId = (r.workerNodeIdOpt |> Option.bind (fun e -> Some e.value.value)),
    //                        RunQueueStatusId = r.runQueueStatus.value,
    //                        ErrorMessage = (r.progressData.progressData.errorMessageOpt |> Option.bind (fun e -> Some e.value)),
    //                        Progress = r.progressData.progressData.progress,
    //                        CallCount = r.progressData.progressData.callCount,
    //                        RelativeInvariant = r.progressData.yRelative,
    //                        ModifiedOn = DateTime.Now)

    //    row


    //let private addClmRunQueueRow (ctx : ClmContext) (r : RunQueue) =
    //    let row = ctx.Clm.RunQueue.Create(
    //                        RunQueueId = r.runQueueId.value,
    //                        ModelDataId = r.info.modelDataId.value,
    //                        Y0 = r.modelCommandLineParam.y0,
    //                        TEnd = r.modelCommandLineParam.tEnd,
    //                        UseAbundant = r.modelCommandLineParam.useAbundant,
    //                        MaxEe = r.progressData.eeData.maxEe,
    //                        MaxAverageEe = r.progressData.eeData.maxAverageEe,
    //                        MaxWeightedAverageAbsEe = r.progressData.eeData.maxWeightedAverageAbsEe,
    //                        MaxLastEe = r.progressData.eeData.maxLastEe)

    //    row


    //let saveRunQueue c modelDataId defaultValueId p =
    //    let g() =
    //        let ctx = getDbContext c
    //        let r = RunQueue.fromModelCommandLineParam modelDataId defaultValueId p
    //        let row = addRunQueueRow ctx r
    //        ctx.SubmitUpdates()
    //        row.RunQueueId |> RunQueueId |> Ok

    //    tryDbFun g


    //let deleteRunQueue c (q : RunQueueId) =
    //    let g() =
    //        let ctx = getDbContext c
    //        let r = ctx.Procedures.DeleteRunQueue.Invoke q.value
    //        let m = r.ResultSet |> mapIntScalar

    //        match m with
    //        | Some 1 -> Ok ()
    //        | _ -> toError DeleteRunQueueEntryErr q

    //    tryDbFun g


    ///// The following transitions are allowed here:
    /////
    /////     NotStartedRunQueue + None (workerNodeId) -> RunRequestedRunQueue + Some workerNodeId - scheduled (but not yet confirmed) new work.
    /////     NotStartedRunQueue + None (workerNodeId) -> CancelledRunQueue + None (workerNodeId) - cancelled work that has not been scheduled yet.
    /////
    /////     RunRequestedRunQueue + Some workerNodeId -> NotStartedRunQueue + None (workerNodeId) - the node rejected work.
    /////     RunRequestedRunQueue + Some workerNodeId -> InProgressRunQueue + the same Some workerNodeId - the node accepted work.
    /////     RunRequestedRunQueue + Some workerNodeId -> CancelRequestedRunQueue + the same Some workerNodeId -
    /////          scheduled (but not yet confirmed) new work, which then was requested to be cancelled before the node replied.
    /////     + -> completed / failed
    /////
    /////     InProgressRunQueue -> InProgressRunQueue + the same Some workerNodeId - normal work progress.
    /////     InProgressRunQueue -> CompletedRunQueue + the same Some workerNodeId (+ the progress will be updated to Completed _) - completed work.
    /////     InProgressRunQueue -> FailedRunQueue + the same Some workerNodeId - failed work.
    /////     InProgressRunQueue -> CancelRequestedRunQueue + the same Some workerNodeId - request for cancellation of actively running work.
    /////
    /////     CancelRequestedRunQueue -> CancelRequestedRunQueue + the same Some workerNodeId - repeated cancel request.
    /////     CancelRequestedRunQueue -> InProgressRunQueue + the same Some workerNodeId -
    /////         roll back to cancel requested - in progress message came while our cancel request propagates through the system.
    /////     CancelRequestedRunQueue -> CancelledRunQueue + the same Some workerNodeId - the work has been successfully cancelled.
    /////     CancelRequestedRunQueue -> CompletedRunQueue + the same Some workerNodeId - the node completed work before cancel request propagated through the system.
    /////     CancelRequestedRunQueue -> FailedRunQueue + the same Some workerNodeId - the node failed before cancel request propagated through the system.
    /////
    ///// All others are not allowed and / or out of scope of this function.
    //let private tryUpdateRunQueueRow (r : RunQueueEntity) (q : RunQueue) =
    //    let toError e = e |> RunQueueTryUpdateRowErr |> DbErr |> Error

    //    let g s u =
    //        //r.RunQueueId <- q.runQueueId.value
    //        r.WorkerNodeId <- (q.workerNodeIdOpt |> Option.bind (fun e -> Some e.value.value))
    //        r.Progress <- q.progressData.progressData.progress
    //        r.CallCount <- q.progressData.progressData.callCount
    //        r.RelativeInvariant <- q.progressData.yRelative
    //        r.ErrorMessage <- q.progressData.progressData.errorMessageOpt |> Option.bind (fun e -> Some e.value)

    //        // r.YRelative <- q.progressData.yRelative
    //        // r.MaxEe <- q.progressData.eeData.maxEe
    //        // r.MaxAverageEe <- q.progressData.eeData.maxAverageEe
    //        // r.MaxWeightedAverageAbsEe <- q.progressData.eeData.maxWeightedAverageAbsEe
    //        // r.MaxLastEe <- q.progressData.eeData.maxLastEe

    //        match s with
    //        | Some (Some v) -> r.StartedOn <- Some v
    //        | Some None-> r.StartedOn <- None
    //        | None -> ()

    //        r.ModifiedOn <- DateTime.Now

    //        match u with
    //        | true -> r.RunQueueStatusId <- q.runQueueStatus.value
    //        | false -> ()

    //        Ok()

    //    let f s =
    //        {
    //            runQueueId = q.runQueueId
    //            runQueueStatusFrom = s
    //            runQueueStatusTo = q.runQueueStatus
    //            workerNodeIdOptFrom = r.WorkerNodeId |> Option.bind (fun e -> e |> MessagingClientId |> WorkerNodeId |> Some)
    //            workerNodeIdOptTo = q.workerNodeIdOpt
    //        }

    //    let f1 e = e |> InvalidStatusTransitionErr |> toError
    //    let f2 e = e |> InvalidDataErr |> toError

    //    match RunQueueStatus.tryCreate r.RunQueueStatusId with
    //    | Some s ->
    //        match s, r.WorkerNodeId, q.runQueueStatus, q.workerNodeIdOpt with
    //        | NotStartedRunQueue,       None,    RunRequestedRunQueue,   Some _ -> g (Some (Some DateTime.Now)) true
    //        | NotStartedRunQueue,       None,    CancelledRunQueue,      None -> g None true

    //        | RunRequestedRunQueue,   Some _, NotStartedRunQueue,       None -> g (Some None) true
    //        | RunRequestedRunQueue,   Some w1, InProgressRunQueue,       Some w2 when w1 = w2.value.value -> g None true
    //        | RunRequestedRunQueue,   Some w1, CancelRequestedRunQueue,  Some w2 when w1 = w2.value.value -> g None true
    //        | RunRequestedRunQueue,   Some w1, CompletedRunQueue,        Some w2 when w1 = w2.value.value -> g None true
    //        | RunRequestedRunQueue,   Some w1, FailedRunQueue,           Some w2 when w1 = w2.value.value -> g None true

    //        | InProgressRunQueue,      Some w1, InProgressRunQueue,      Some w2 when w1 = w2.value.value -> g None true
    //        | InProgressRunQueue,      Some w1, CompletedRunQueue,       Some w2 when w1 = w2.value.value -> g None true
    //        | InProgressRunQueue,      Some w1, FailedRunQueue,          Some w2 when w1 = w2.value.value -> g None true
    //        | InProgressRunQueue,      Some w1, CancelRequestedRunQueue, Some w2 when w1 = w2.value.value -> g None true

    //        | CancelRequestedRunQueue, Some w1, CancelRequestedRunQueue, Some w2 when w1 = w2.value.value -> g None true
    //        | CancelRequestedRunQueue, Some w1, InProgressRunQueue,      Some w2 when w1 = w2.value.value -> g None false // !!! Roll back the status change !!!
    //        | CancelRequestedRunQueue, Some w1, CancelledRunQueue,       Some w2 when w1 = w2.value.value -> g None true
    //        | CancelRequestedRunQueue, Some w1, CompletedRunQueue,       Some w2 when w1 = w2.value.value -> g None true
    //        | CancelRequestedRunQueue, Some w1, FailedRunQueue,          Some w2 when w1 = w2.value.value -> g None true
    //        | _ -> s |> Some |> f |> f1
    //    | None -> None |> f |> f2


    //let upsertRunQueue c (w : RunQueue) =
    //    let g() =
    //        let ctx = getDbContext c

    //        let x =
    //            query {
    //                for r in ctx.Dbo.RunQueue do
    //                where (r.RunQueueId = w.runQueueId.value)
    //                select (Some r)
    //                exactlyOneOrDefault
    //            }

    //        let result =
    //            match x with
    //            | Some v -> tryUpdateRunQueueRow v w
    //            | None -> addRunQueueRow ctx w |> ignore; Ok()

    //        ctx.SubmitUpdates()
    //        result

    //    tryDbFun g


    //let private createWorkerNodeInfo p (r : WorkerNodeEntity) =
    //    {
    //        workerNodeId = r.WorkerNodeId |> MessagingClientId |> WorkerNodeId
    //        workerNodeName = r.WorkerNodeName |> WorkerNodeName
    //        noOfCores = r.NumberOfCores
    //        partitionerId = p
    //        nodePriority = r.NodePriority |> WorkerNodePriority
    //        isInactive = r.IsInactive
    //        lastErrorDateOpt = r.LastErrorOn
    //    }


    //let loadWorkerNodeInfo c p (i : WorkerNodeId) =
    //    let g() =
    //        let ctx = getDbContext c

    //        let x =
    //            query {
    //                for w in ctx.Dbo.WorkerNode do
    //                where (w.WorkerNodeId = i.value.value)
    //                select (Some w)
    //                exactlyOneOrDefault
    //            }

    //        match x with
    //        | Some v -> v |> createWorkerNodeInfo p |> Ok
    //        | None -> toError LoadWorkerNodeInfoErr i.value.value

    //    tryDbFun g


    //let private updateWorkerNodeRow (r : WorkerNodeEntity) (w : WorkerNodeInfo) =
    //    r.WorkerNodeName <- w.workerNodeName.value
    //    r.NumberOfCores <- w.noOfCores
    //    r.NodePriority <- w.nodePriority.value
    //    r.ModifiedOn <- DateTime.Now
    //    r.LastErrorOn <- w.lastErrorDateOpt

    //    Ok()


    //let private addWorkerNodeRow (ctx : ClmContext) (w : WorkerNodeInfo) =
    //    let row = ctx.Dbo.WorkerNode.Create(
    //                        WorkerNodeId = w.workerNodeId.value.value,
    //                        WorkerNodeName = w.workerNodeName.value,
    //                        NumberOfCores = w.noOfCores,
    //                        NodePriority = w.nodePriority.value,
    //                        LastErrorOn = w.lastErrorDateOpt,
    //                        ModifiedOn = DateTime.Now)

    //    row


    //let upsertWorkerNodeInfo c (w : WorkerNodeInfo) =
    //    let g() =
    //        let ctx = getDbContext c

    //        let x =
    //            query {
    //                for r in ctx.Dbo.WorkerNode do
    //                where (r.WorkerNodeId = w.workerNodeId.value.value)
    //                select (Some r)
    //                exactlyOneOrDefault
    //            }

    //        let result =
    //            match x with
    //            | Some v -> updateWorkerNodeRow v w
    //            | None -> addWorkerNodeRow ctx w |> ignore; Ok()

    //        ctx.SubmitUpdates()
    //        result

    //    tryDbFun g


    //let upsertWorkerNodeErr c p i =
    //    let g() =
    //        match loadWorkerNodeInfo c p i with
    //        | Ok w -> upsertWorkerNodeInfo c { w with lastErrorDateOpt = Some DateTime.Now }
    //        | Error e -> Error e

    //    tryDbFun g


    ///// Gets the first available worker node to schedule work.
    //let tryGetAvailableWorkerNode c (LastAllowedNodeErr m) =
    //    let g() =
    //        let ctx = getDbContext c

    //        let x =
    //            query {
    //                for r in ctx.Dbo.VwAvailableWorkerNode do
    //                where (r.WorkLoad < 1.0m && (r.LastErrMinAgo = None || r.LastErrMinAgo.Value < (m / 1<minute>)))
    //                sortByDescending r.NodePriority
    //                thenBy r.WorkLoad
    //                thenBy r.OrderId
    //                select (Some r)
    //                headOrDefault
    //            }

    //        match x with
    //        | Some r -> r.WorkerNodeId |> MessagingClientId |> WorkerNodeId |> Some |> Ok
    //        | None -> Ok None

    //    tryDbFun g
