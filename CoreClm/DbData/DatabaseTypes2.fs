#nowarn "1104"

namespace DbData

open Newtonsoft.Json
open FSharp.Data.Sql
open System

open Primitives.GeneralPrimitives
open Softellect.Sys
open Softellect.Sys.Core
open Softellect.Sys.MessagingPrimitives

open Clm.Substances
open Clm.ModelParams
open Clm.CalculationData
open Clm.ReactionRates
open ClmSys.GeneralErrors
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.WorkerNodeData
open ClmSys.PartitionerData

// ! Must be the last to open !
open DbData.Configuration

module DatabaseTypes =

    type private ClmDb = SqlDataProvider<
                    Common.DatabaseProviderTypes.MSSQLSERVER,
                    ConnectionString = ContGenConnectionStringValue,
                    UseOptionTypes = Common.NullableColumnType.OPTION>


    type private ClmContext = ClmDb.dataContext
    let private getDbContext (c : unit -> ConnectionString) = c().value |> ClmDb.GetDataContext


    type private ClmDefaultValueEntity = ClmContext.``dbo.ClmDefaultValueEntity``
    type private ClmTaskEntity = ClmContext.``dbo.ClmTaskEntity``
    type private CommandLineParamEntity = ClmContext.``dbo.CommandLineParamEntity``
    type private ModelDataEntity = ClmContext.``dbo.ModelDataEntity``
    type private RunQueueEntity = ClmContext.``dbo.RunQueueEntity``
    type private WorkerNodeEntity = ClmContext.``dbo.WorkerNodeEntity``


    type private RunQueueTableData =
        {
            runQueue : RunQueueEntity
            clmDefaultValueId : int64
        }



    let private createClmDefaultValue (r : ClmDefaultValueEntity) =
        {
            clmDefaultValueId = r.ClmDefaultValueId |> ClmDefaultValueId
            defaultRateParams = r.DefaultRateParams |> JsonConvert.DeserializeObject<ReactionRateProviderParams>
            description = r.Description
        }


    let loadClmDefaultValue c (ClmDefaultValueId clmDefaultValueId) =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for c in ctx.Dbo.ClmDefaultValue do
                    where (c.ClmDefaultValueId = clmDefaultValueId)
                    select (Some c)
                    exactlyOneOrDefault
                }

            x
            |> Option.map createClmDefaultValue
            |> mapDbError LoadClmDefaultValueErr clmDefaultValueId

        tryDbFun g


    let upsertClmDefaultValue c (p : ClmDefaultValue) =
        let g() =
            let ctx = getDbContext c

            let r = ctx.Procedures.UpsertClmDefaultValue.Invoke(
                            ``@clmDefaultValueId`` = p.clmDefaultValueId.value,
                            ``@defaultRateParams`` = (p.defaultRateParams |> JsonConvert.SerializeObject),
                            ``@description`` = (match p.description with | Some d -> d | None -> null))

            r.ResultSet |> bindIntScalar UpsertClmDefaultValueErr p.clmDefaultValueId.value

        tryDbFun g


    let private createModelCommandLineParam (r : CommandLineParamEntity) =
        {
            y0 = r.Y0
            tEnd = r.TEnd
            useAbundant = r.UseAbundant
        }


    let loadCommandLineParams c (ClmTaskId clmTaskId) =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for p in ctx.Dbo.CommandLineParam do
                    where (p.ClmTaskId = clmTaskId)
                    sortBy p.CreatedOn
                    select p
                }

            x
            |> Seq.toList
            |> List.map createModelCommandLineParam
            |> Ok

        tryDbFun g


    let private addCommandLineParamRow (ctx : ClmContext) (ClmTaskId clmTaskId) (p : ModelCommandLineParam) =
        let row = ctx.Dbo.CommandLineParam.Create(
                            ClmTaskId = clmTaskId,
                            CommandLineParamId = Guid.NewGuid(),
                            Y0 = p.y0,
                            TEnd = p.tEnd,
                            UseAbundant = p.useAbundant)

        row


    let addCommandLineParams c clmTaskId (p : ModelCommandLineParam) =
        let g() =
            let ctx = getDbContext c
            let row = addCommandLineParamRow ctx clmTaskId p
            ctx.SubmitUpdates()
            Ok()

        tryDbFun g


    let private tryCreateClmTask c (r : ClmTaskEntity) =
        match r.NumberOfAminoAcids |> NumberOfAminoAcids.tryCreate, r.MaxPeptideLength |> MaxPeptideLength.tryCreate with
        | Some n, Some m ->
            let clmTaskId = r.ClmTaskId |> ClmTaskId

            match c clmTaskId with
            | Ok p ->
                {
                    clmTaskInfo =
                        {
                            clmTaskId = clmTaskId

                            taskDetails =
                                {
                                    clmDefaultValueId = r.ClmDefaultValueId |> ClmDefaultValueId
                                    clmTaskPriority = r.ClmTaskPriority |> ClmTaskPriority
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
            | Error e -> addError ClmTaskTryCreatErr r.ClmTaskId e
        | _ -> toError ClmTaskTryCreatErr r.ClmTaskId


    let loadClmTask c (ClmTaskId clmTaskId) =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for c in ctx.Dbo.ClmTask do
                    where (c.ClmTaskId = clmTaskId)
                    select (Some c)
                    exactlyOneOrDefault
                }

            match x with
            | Some v -> tryCreateClmTask (loadCommandLineParams c) v
            | None -> toError LoadClmTaskErr clmTaskId

        tryDbFun g


    let loadIncompleteClmTasks c =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for c in ctx.Dbo.ClmTask do
                    where (c.RemainingRepetitions > 0 && c.ClmTaskStatusId = 0)
                    select c
                }

            x
            |> Seq.toList
            |> List.map (fun r -> tryCreateClmTask (loadCommandLineParams c) r)
            |> Ok

        tryDbFun g


    let private addClmTaskRow  (ctx : ClmContext) (r : ClmTask) =
        let row = ctx.Dbo.ClmTask.Create(
                            ClmTaskId = r.clmTaskInfo.clmTaskId.value,
                            ClmDefaultValueId = r.clmTaskInfo.taskDetails.clmDefaultValueId.value,
                            ClmTaskPriority = r.clmTaskInfo.taskDetails.clmTaskPriority.value,
                            NumberOfAminoAcids = r.clmTaskInfo.taskDetails.numberOfAminoAcids.length,
                            MaxPeptideLength = r.clmTaskInfo.taskDetails.maxPeptideLength.length,
                            NumberOfRepetitions = r.numberOfRepetitions,
                            RemainingRepetitions = r.remainingRepetitions,
                            CreatedOn = DateTime.Now)

        row


    let addClmTask c (clmTask : ClmTask) =
        let g() =
            let ctx = getDbContext c
            let row = addClmTaskRow ctx clmTask
            ctx.SubmitUpdates()

            let result =
                clmTask.commandLineParams
                |> List.map (addCommandLineParams c clmTask.clmTaskInfo.clmTaskId)
                |> foldUnitResults

            result

        tryDbFun g


    /// Updates remainingRepetitions of ClmTask.
    let updateClmTask c (clmTask : ClmTask) =
        let g() =
            let ctx = getDbContext c

            let r = ctx.Procedures.UpdateClmTask.Invoke(
                            ``@clmTaskId`` = clmTask.clmTaskInfo.clmTaskId.value,
                            ``@remainingRepetitions`` = clmTask.remainingRepetitions)

            let m = r.ResultSet |> mapIntScalar

            match m with
            | Some 1 -> Ok ()
            | _ -> toError UpdateClmTaskErr clmTask.clmTaskInfo.clmTaskId.value

        tryDbFun g


    let private tryCreateModelData (c : ClmTaskId -> ClmResult<ClmTask>) (r : ModelDataEntity) =
        match r.ClmTaskId |> ClmTaskId |> c with
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
                modelDataId = r.ModelDataId |> ModelDataId
                clmTaskInfo = i.clmTaskInfo
                data = rawData
            }
            |> Ok
        | Error e ->  addError ModelDataTryCreateErr r.ModelDataId e


    let loadModelData c (ModelDataId modelDataId) =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for m in ctx.Dbo.ModelData do
                    where (m.ModelDataId = modelDataId)
                    select (Some m)
                    exactlyOneOrDefault
                }

            match x with
            | Some v -> tryCreateModelData (loadClmTask c) v
            | None -> toError LoadModelDataError modelDataId

        tryDbFun g


    let upsertModelData c (m : ModelData) =
        let g() =
            let ctx = getDbContext c

            let r = ctx.Procedures.UpsertModelData.Invoke(
                        ``@modelDataId`` = m.modelDataId.value,
                        ``@clmTaskId`` = m.clmTaskInfo.clmTaskId.value,
                        ``@seedValue`` = (match m.data.seedValue with | Some s -> s | None -> -1),
                        ``@modelDataParams`` = (m.data.modelData.modelDataParams |> JsonConvert.SerializeObject),
                        ``@modelBinaryData`` = (m.data.modelData.modelBinaryData |> JsonConvert.SerializeObject |> zip),
                        ``@createdOn`` = DateTime.Now)


            let x = r.ResultSet |> mapIntScalar

            match x with
            | Some 1 -> Ok ()
            | _ -> toError UpdateModelDataErr m.modelDataId.value

        tryDbFun g


    let private mapRunQueue (r: RunQueueTableData) =
        match RunQueueStatus.tryCreate r.runQueue.RunQueueStatusId with
        | Some s ->
            {
                runQueueId = RunQueueId r.runQueue.RunQueueId

                info =
                    {
                        modelDataId = ModelDataId r.runQueue.ModelDataId
                        defaultValueId = ClmDefaultValueId r.clmDefaultValueId

                        modelCommandLineParam =
                            {
                                y0 = r.runQueue.Y0
                                tEnd = r.runQueue.TEnd
                                useAbundant = r.runQueue.UseAbundant
                            }
                    }

                runQueueStatus = s
                workerNodeIdOpt = r.runQueue.WorkerNodeId |> Option.bind (fun e -> e |> MessagingClientId |> WorkerNodeId |> Some)

                progressData =
                    {
                        progressData =
                            {
                                progress = r.runQueue.Progress
                                callCount = r.runQueue.CallCount
                                errorMessageOpt = r.runQueue.ErrorMessage |> Option.map ErrorMessage
                            }
                        yRelative = r.runQueue.YRelative

                        eeData =
                            {
                                maxEe = r.runQueue.MaxEe
                                maxAverageEe = r.runQueue.MaxAverageEe
                                maxWeightedAverageAbsEe = r.runQueue.MaxWeightedAverageAbsEe
                                maxLastEe = r.runQueue.MaxLastEe
                            }
                    }

                createdOn = r.runQueue.CreatedOn
            }
            |> Ok
        | None -> toError MapRunQueueErr (r.runQueue.RunQueueId)


    let private mapRunQueueResults x =
        x
        |> List.ofSeq
        |> List.map (fun e -> { runQueue = fst e; clmDefaultValueId = snd e })
        |> List.map mapRunQueue


    /// Loads all not started RunQueue.
    let loadRunQueue c =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for r in ctx.Dbo.RunQueue do
                    join m in ctx.Dbo.ModelData on (r.ModelDataId = m.ModelDataId)
                    join t in ctx.Dbo.ClmTask on (m.ClmTaskId = t.ClmTaskId)
                    where (r.RunQueueStatusId = 0 && r.Progress = 0.0m && t.ClmTaskStatusId = 0 && r.WorkerNodeId = None)
                    select (r, t.ClmDefaultValueId)
                }

            mapRunQueueResults x |> Ok

        tryDbFun g


    /// Loads all currently running models == total progress.
    /// RunQueueStatusId = 2 is InProgressRunQueue.
    let loadRunQueueProgress c =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for r in ctx.Dbo.RunQueue do
                    join m in ctx.Dbo.ModelData on (r.ModelDataId = m.ModelDataId)
                    join t in ctx.Dbo.ClmTask on (m.ClmTaskId = t.ClmTaskId)
                    where (r.RunQueueStatusId = 2 && t.ClmTaskStatusId = 0 && r.WorkerNodeId <> None)
                    sortBy t.ClmTaskPriority
                    thenBy r.RunQueueOrder
                    select (r, t.ClmDefaultValueId)
                }

            mapRunQueueResults x |> Ok

        tryDbFun g


    /// Loads first not started RunQueue.
    let tryLoadFirstRunQueue c =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for r in ctx.Dbo.RunQueue do
                    join m in ctx.Dbo.ModelData on (r.ModelDataId = m.ModelDataId)
                    join t in ctx.Dbo.ClmTask on (m.ClmTaskId = t.ClmTaskId)
                    where (r.RunQueueStatusId = 0 && r.Progress = 0.0m && t.ClmTaskStatusId = 0 && r.WorkerNodeId = None)
                    sortBy t.ClmTaskPriority
                    thenBy r.RunQueueOrder
                    select (r, t.ClmDefaultValueId)
                }

            mapRunQueueResults x |> List.tryHead |> Ok

        tryDbFun g |> Rop.unwrapResultOption


    /// Loads RunQueue by runQueueId.
    let tryLoadRunQueue c (q : RunQueueId) =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for r in ctx.Dbo.RunQueue do
                    join m in ctx.Dbo.ModelData on (r.ModelDataId = m.ModelDataId)
                    join t in ctx.Dbo.ClmTask on (m.ClmTaskId = t.ClmTaskId)
                    where (r.RunQueueId = q.value)
                    select (r, t.ClmDefaultValueId)
                }

            mapRunQueueResults x |> List.tryHead |> Ok

        tryDbFun g |> Rop.unwrapResultOption


    /// Tries to reset RunQueue.
    let tryResetRunQueue c (q : RunQueueId) =
        let g() =
            let ctx = getDbContext c
            let r = ctx.Procedures.TryResetRunQueue.Invoke q.value
            let m = r.ResultSet |> mapIntScalar

            match m with
            | Some 1 -> Ok ()
            | _ -> toError ResetRunQueueEntryErr q

        tryDbFun g


    let private addRunQueueRow (ctx : ClmContext) (r : RunQueue) =
        let row = ctx.Dbo.RunQueue.Create(
                            RunQueueId = r.runQueueId.value,
                            ModelDataId = r.info.modelDataId.value,
                            RunQueueStatusId = r.runQueueStatus.value,
                            Y0 = r.modelCommandLineParam.y0,
                            TEnd = r.modelCommandLineParam.tEnd,
                            UseAbundant = r.modelCommandLineParam.useAbundant,
                            Progress = r.progressData.progressData.progress,
                            CallCount = r.progressData.progressData.callCount,
                            YRelative = r.progressData.yRelative,
                            MaxEe = r.progressData.eeData.maxEe,
                            MaxAverageEe = r.progressData.eeData.maxAverageEe,
                            MaxWeightedAverageAbsEe = r.progressData.eeData.maxWeightedAverageAbsEe,
                            MaxLastEe = r.progressData.eeData.maxLastEe,
                            WorkerNodeId = (r.workerNodeIdOpt |> Option.bind (fun e -> Some e.value.value)),
                            ModifiedOn = DateTime.Now,
                            ErrorMessage = (r.progressData.progressData.errorMessageOpt |> Option.bind (fun e -> Some e.value)))

        row


    let saveRunQueue c modelDataId defaultValueId p =
        let g() =
            let ctx = getDbContext c
            let r = RunQueue.fromModelCommandLineParam modelDataId defaultValueId p
            let row = addRunQueueRow ctx r
            ctx.SubmitUpdates()
            row.RunQueueId |> RunQueueId |> Ok

        tryDbFun g


    let deleteRunQueue c (q : RunQueueId) =
        let g() =
            let ctx = getDbContext c
            let r = ctx.Procedures.DeleteRunQueue.Invoke q.value
            let m = r.ResultSet |> mapIntScalar

            match m with
            | Some 1 -> Ok ()
            | _ -> toError DeleteRunQueueEntryErr q

        tryDbFun g


    /// The following transitions are allowed here:
    ///
    ///     NotStartedRunQueue + None (workerNodeId) -> RunRequestedRunQueue + Some workerNodeId - scheduled (but not yet confirmed) new work.
    ///     NotStartedRunQueue + None (workerNodeId) -> CancelledRunQueue + None (workerNodeId) - cancelled work that has not been scheduled yet.
    ///
    ///     RunRequestedRunQueue + Some workerNodeId -> NotStartedRunQueue + None (workerNodeId) - the node rejected work.
    ///     RunRequestedRunQueue + Some workerNodeId -> InProgressRunQueue + the same Some workerNodeId - the node accepted work.
    ///     RunRequestedRunQueue + Some workerNodeId -> CancelRequestedRunQueue + the same Some workerNodeId -
    ///          scheduled (but not yet confirmed) new work, which then was requested to be cancelled before the node replied.
    ///     + -> completed / failed
    ///
    ///     InProgressRunQueue -> InProgressRunQueue + the same Some workerNodeId - normal work progress.
    ///     InProgressRunQueue -> CompletedRunQueue + the same Some workerNodeId (+ the progress will be updated to Completed _) - completed work.
    ///     InProgressRunQueue -> FailedRunQueue + the same Some workerNodeId - failed work.
    ///     InProgressRunQueue -> CancelRequestedRunQueue + the same Some workerNodeId - request for cancellation of actively running work.
    ///
    ///     CancelRequestedRunQueue -> CancelRequestedRunQueue + the same Some workerNodeId - repeated cancel request.
    ///     CancelRequestedRunQueue -> InProgressRunQueue + the same Some workerNodeId -
    ///         roll back to cancel requested - in progress message came while our cancel request propagates through the system.
    ///     CancelRequestedRunQueue -> CancelledRunQueue + the same Some workerNodeId - the work has been successfully cancelled.
    ///     CancelRequestedRunQueue -> CompletedRunQueue + the same Some workerNodeId - the node completed work before cancel request propagated through the system.
    ///     CancelRequestedRunQueue -> FailedRunQueue + the same Some workerNodeId - the node failed before cancel request propagated through the system.
    ///
    /// All others are not allowed and / or out of scope of this function.
    let private tryUpdateRunQueueRow (r : RunQueueEntity) (q : RunQueue) =
        let toError e = e |> RunQueueTryUpdateRowErr |> DbErr |> Error

        let g s u =
            //r.RunQueueId <- q.runQueueId.value
            r.WorkerNodeId <- (q.workerNodeIdOpt |> Option.bind (fun e -> Some e.value.value))
            r.Progress <- q.progressData.progressData.progress
            r.CallCount <- q.progressData.progressData.callCount
            r.YRelative <- q.progressData.yRelative
            r.MaxEe <- q.progressData.eeData.maxEe
            r.MaxAverageEe <- q.progressData.eeData.maxAverageEe
            r.MaxWeightedAverageAbsEe <- q.progressData.eeData.maxWeightedAverageAbsEe
            r.MaxLastEe <- q.progressData.eeData.maxLastEe
            r.ErrorMessage <- q.progressData.progressData.errorMessageOpt |> Option.bind (fun e -> Some e.value)

            match s with
            | Some (Some v) -> r.StartedOn <- Some v
            | Some None-> r.StartedOn <- None
            | None -> ()

            r.ModifiedOn <- DateTime.Now

            match u with
            | true -> r.RunQueueStatusId <- q.runQueueStatus.value
            | false -> ()

            Ok()

        let f s =
            {
                runQueueId = q.runQueueId
                runQueueStatusFrom = s
                runQueueStatusTo = q.runQueueStatus
                workerNodeIdOptFrom = r.WorkerNodeId |> Option.bind (fun e -> e |> MessagingClientId |> WorkerNodeId |> Some)
                workerNodeIdOptTo = q.workerNodeIdOpt
            }

        let f1 e = e |> InvalidStatusTransitionErr |> toError
        let f2 e = e |> InvalidDataErr |> toError

        match RunQueueStatus.tryCreate r.RunQueueStatusId with
        | Some s ->
            match s, r.WorkerNodeId, q.runQueueStatus, q.workerNodeIdOpt with
            | NotStartedRunQueue,       None,    RunRequestedRunQueue,   Some _ -> g (Some (Some DateTime.Now)) true
            | NotStartedRunQueue,       None,    CancelledRunQueue,      None -> g None true

            | RunRequestedRunQueue,   Some _, NotStartedRunQueue,       None -> g (Some None) true
            | RunRequestedRunQueue,   Some w1, InProgressRunQueue,       Some w2 when w1 = w2.value.value -> g None true
            | RunRequestedRunQueue,   Some w1, CancelRequestedRunQueue,  Some w2 when w1 = w2.value.value -> g None true
            | RunRequestedRunQueue,   Some w1, CompletedRunQueue,        Some w2 when w1 = w2.value.value -> g None true
            | RunRequestedRunQueue,   Some w1, FailedRunQueue,           Some w2 when w1 = w2.value.value -> g None true

            | InProgressRunQueue,      Some w1, InProgressRunQueue,      Some w2 when w1 = w2.value.value -> g None true
            | InProgressRunQueue,      Some w1, CompletedRunQueue,       Some w2 when w1 = w2.value.value -> g None true
            | InProgressRunQueue,      Some w1, FailedRunQueue,          Some w2 when w1 = w2.value.value -> g None true
            | InProgressRunQueue,      Some w1, CancelRequestedRunQueue, Some w2 when w1 = w2.value.value -> g None true

            | CancelRequestedRunQueue, Some w1, CancelRequestedRunQueue, Some w2 when w1 = w2.value.value -> g None true
            | CancelRequestedRunQueue, Some w1, InProgressRunQueue,      Some w2 when w1 = w2.value.value -> g None false // !!! Roll back the status change !!!
            | CancelRequestedRunQueue, Some w1, CancelledRunQueue,       Some w2 when w1 = w2.value.value -> g None true
            | CancelRequestedRunQueue, Some w1, CompletedRunQueue,       Some w2 when w1 = w2.value.value -> g None true
            | CancelRequestedRunQueue, Some w1, FailedRunQueue,          Some w2 when w1 = w2.value.value -> g None true
            | _ -> s |> Some |> f |> f1
        | None -> None |> f |> f2


    let upsertRunQueue c (w : RunQueue) =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for r in ctx.Dbo.RunQueue do
                    where (r.RunQueueId = w.runQueueId.value)
                    select (Some r)
                    exactlyOneOrDefault
                }

            let result =
                match x with
                | Some v -> tryUpdateRunQueueRow v w
                | None -> addRunQueueRow ctx w |> ignore; Ok()

            ctx.SubmitUpdates()
            result

        tryDbFun g


    let private createWorkerNodeInfo p (r : WorkerNodeEntity) =
        {
            workerNodeId = r.WorkerNodeId |> MessagingClientId |> WorkerNodeId
            workerNodeName = r.WorkerNodeName |> WorkerNodeName
            noOfCores = r.NumberOfCores
            partitionerId = p
            nodePriority = r.NodePriority |> WorkerNodePriority
            isInactive = r.IsInactive
            lastErrorDateOpt = r.LastErrorOn
        }


    let loadWorkerNodeInfo c p (i : WorkerNodeId) =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for w in ctx.Dbo.WorkerNode do
                    where (w.WorkerNodeId = i.value.value)
                    select (Some w)
                    exactlyOneOrDefault
                }

            match x with
            | Some v -> v |> createWorkerNodeInfo p |> Ok
            | None -> toError LoadWorkerNodeInfoErr i.value.value

        tryDbFun g


    let private updateWorkerNodeRow (r : WorkerNodeEntity) (w : WorkerNodeInfo) =
        r.WorkerNodeName <- w.workerNodeName.value
        r.NumberOfCores <- w.noOfCores
        r.NodePriority <- w.nodePriority.value
        r.ModifiedOn <- DateTime.Now
        r.LastErrorOn <- w.lastErrorDateOpt

        Ok()


    let private addWorkerNodeRow (ctx : ClmContext) (w : WorkerNodeInfo) =
        let row = ctx.Dbo.WorkerNode.Create(
                            WorkerNodeId = w.workerNodeId.value.value,
                            WorkerNodeName = w.workerNodeName.value,
                            NumberOfCores = w.noOfCores,
                            NodePriority = w.nodePriority.value,
                            LastErrorOn = w.lastErrorDateOpt,
                            ModifiedOn = DateTime.Now)

        row


    let upsertWorkerNodeInfo c (w : WorkerNodeInfo) =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for r in ctx.Dbo.WorkerNode do
                    where (r.WorkerNodeId = w.workerNodeId.value.value)
                    select (Some r)
                    exactlyOneOrDefault
                }

            let result =
                match x with
                | Some v -> updateWorkerNodeRow v w
                | None -> addWorkerNodeRow ctx w |> ignore; Ok()

            ctx.SubmitUpdates()
            result

        tryDbFun g


    let upsertWorkerNodeErr c p i =
        let g() =
            match loadWorkerNodeInfo c p i with
            | Ok w -> upsertWorkerNodeInfo c { w with lastErrorDateOpt = Some DateTime.Now }
            | Error e -> Error e

        tryDbFun g


    /// Gets the first available worker node to schedule work.
    let tryGetAvailableWorkerNode c (LastAllowedNodeErr m) =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for r in ctx.Dbo.VwAvailableWorkerNode do
                    where (r.WorkLoad < 1.0m && (r.LastErrMinAgo = None || r.LastErrMinAgo.Value < (m / 1<minute>)))
                    sortByDescending r.NodePriority
                    thenBy r.WorkLoad
                    thenBy r.OrderId
                    select (Some r)
                    headOrDefault
                }

            match x with
            | Some r -> r.WorkerNodeId |> MessagingClientId |> WorkerNodeId |> Some |> Ok
            | None -> Ok None

        tryDbFun g
