#nowarn "1104"

namespace DbData

open Newtonsoft.Json
open FSharp.Data.Sql
open System

//open Primitives.GeneralPrimitives
open Softellect.Sys
open Softellect.Sys.Core
open Softellect.Sys.Rop
open Softellect.Sys.DataAccess
open Softellect.Messaging.Primitives
open Softellect.Sys.Primitives

open Clm.ModelParams

open ClmSys.GeneralErrors
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
//open ClmSys.GeneralPrimitives
//open ClmSys.WorkerNodePrimitives
//open ClmSys.WorkerNodeData
//open ClmSys.PartitionerData
open ClmSys.ClmErrors

// ! Must be the last to open !
open DbData.Configuration

module DatabaseTypesDbo =

    type ClmDb = SqlDataProvider<
                    Common.DatabaseProviderTypes.MSSQLSERVER,
                    ConnectionString = ContGenConnectionStringValue,
                    UseOptionTypes = Common.NullableColumnType.OPTION>


    type ClmContext = ClmDb.dataContext
    let getDbContext (c : unit -> ConnectionString) = c().value |> ClmDb.GetDataContext


    type RunQueueEntity = ClmContext.``dbo.RunQueueEntity``
    type WorkerNodeEntity = ClmContext.``dbo.WorkerNodeEntity``


    ///// Tries to reset RunQueue.
    //let tryResetRunQueue c (q : RunQueueId) =
    //    let elevate e = e |> TryResetRunQueueErr
    //    let toError e = e |> elevate |> Error
    //    let fromDbError e = e |> TryResetRunQueueDbErr |> elevate

    //    let g() =
    //        let ctx = getDbContext c
    //        let r = ctx.Procedures.TryResetRunQueue.Invoke q.value
    //        let m = r.ResultSet |> mapIntScalar

    //        match m with
    //        | Some 1 -> Ok ()
    //        | _ -> ResetRunQueueEntryErr q |> toError

    //    tryDbFun fromDbError g


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
    //    let elevate e = e |> SaveRunQueueErr
    //    let toError e = e |> elevate |> Error
    //    let fromDbError e = e |> SaveRunQueueDbErr |> elevate

    //    let g() =
    //        let ctx = getDbContext c
    //        let r = RunQueue.fromModelCommandLineParam modelDataId defaultValueId p
    //        let row = addRunQueueRow ctx r
    //        ctx.SubmitUpdates()
    //        row.RunQueueId |> RunQueueId |> Ok

    //    tryDbFun fromDbError g


    //let deleteRunQueue c (q : RunQueueId) =
    //    let elevate e = e |> DeleteRunQueueErr
    //    let toError e = e |> elevate |> Error
    //    let fromDbError e = e |> DeleteRunQueueDbErr |> elevate

    //    let g() =
    //        let ctx = getDbContext c
    //        let r = ctx.Procedures.DeleteRunQueue.Invoke q.value
    //        let m = r.ResultSet |> mapIntScalar

    //        match m with
    //        | Some 1 -> Ok ()
    //        | _ -> DeleteRunQueueEntryErr q |> toError

    //    tryDbFun fromDbError g


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
    //    let elevate e = e |> TryUpdateRunQueueRowErr
    //    let toError e = e |> elevate |> Error
    //    //let fromDbError e = e |> TryUpdateRunQueueRowDbErr |> elevate

    //    //let toError e = e |> RunQueueTryUpdateRowErr |> DbErr |> Error

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
    //    let elevate e = e |> UpsertRunQueueErr
    //    //let toError e = e |> elevate |> Error
    //    let fromDbError e = e |> UpsertRunQueueDbErr |> elevate

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

    //    tryDbFun fromDbError g


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
    //    let elevate e = e |> LoadWorkerNodeInfoErr
    //    let toError e = e |> elevate |> Error
    //    let fromDbError e = e |> LoadWorkerNodeInfoDbErr |> elevate

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
    //        | None -> UnableToLoadWorkerNodeErr i |> toError

    //    tryDbFun fromDbError g


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
    //    let elevate e = e |> UpsertWorkerNodeInfoErr
    //    //let toError e = e |> elevate |> Error
    //    let fromDbError e = e |> UpsertWorkerNodeInfoDbErr |> elevate

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

    //    tryDbFun fromDbError g


    //let upsertWorkerNodeErr c p i =
    //    let elevate e = e |> UpsertWorkerNodeErrErr
    //    //let toError e = e |> elevate |> Error
    //    let fromDbError e = e |> UpsertWorkerNodeErrDbErr |> elevate

    //    let g() =
    //        match loadWorkerNodeInfo c p i with
    //        | Ok w -> upsertWorkerNodeInfo c { w with lastErrorDateOpt = Some DateTime.Now }
    //        | Error e -> Error e

    //    tryDbFun fromDbError g


    ///// Gets the first available worker node to schedule work.
    //let tryGetAvailableWorkerNode c (LastAllowedNodeErr m) =
    //    let elevate e = e |> TryGetAvailableWorkerNodeErr
    //    //let toError e = e |> elevate |> Error
    //    let fromDbError e = e |> TryGetAvailableWorkerNodeDbErr |> elevate

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

    //    tryDbFun fromDbError g
