#nowarn "1104"

namespace DbData

open ClmSys.WorkerNodeData
open System
open FSharp.Data.Sql
open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives
open Softellect.Sys.Core
open Softellect.Sys.Primitives
open Softellect.Sys.Retry
open ClmSys.GeneralErrors
open ClmSys.ClmErrors
open ClmSys.GeneralPrimitives
open ClmSys.SolverRunnerPrimitives
open MessagingServiceInfo.ServiceInfo

// ! Must be the last to open !
open Configuration

module WorkerNodeDatabaseTypes =

    let serializationFormat = BinaryZippedFormat


    type private WorkerNodeDb = SqlDataProvider<
                    Common.DatabaseProviderTypes.MSSQLSERVER,
                    ConnectionString = WorkerNodeConnectionStringValue,
                    UseOptionTypes = Common.NullableColumnType.OPTION>


    type private WorkerNodeDbContext = WorkerNodeDb.dataContext
    let private getDbContext (c : unit -> ConnectionString) = c().value |> WorkerNodeDb.GetDataContext


    type private RunQueueEntity = WorkerNodeDbContext.``dbo.RunQueueEntity``
    type private MessageEntity = WorkerNodeDbContext.``dbo.MessageEntity``


    let tryLoadSolverRunners c =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for q in ctx.Dbo.RunQueue do
                    where (q.RunQueueStatusId = RunQueueStatus.InProgressRunQueue.value || q.RunQueueStatusId = RunQueueStatus.CancelRequestedRunQueue.value)
                    select (q.RunQueueId, q.ProcessId)
                }

            x
            |> Seq.toList
            |> List.map (fun (r, p) -> { runQueueId = RunQueueId r; processId = p |> Option.bind (fun e -> e |> ProcessId |> Some) })
            |> Ok

        tryDbFun g


    let tryGetRunningSolversCount c =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for q in ctx.Dbo.RunQueue do
                    where (q.RunQueueStatusId = RunQueueStatus.InProgressRunQueue.value || q.RunQueueStatusId = RunQueueStatus.CancelRequestedRunQueue.value)
                    select q
                    count
                }

            Ok x

        tryDbFun g


    let tryPickRunQueue c =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for q in ctx.Dbo.RunQueue do
                    where (q.RunQueueStatusId = RunQueueStatus.NotStartedRunQueue.value)
                    sortBy q.RunQueueOrder
                    select (Some q.RunQueueId)
                    exactlyOneOrDefault
                }

            x |> Option.map RunQueueId |> Ok

        tryDbFun g


    let tryLoadRunQueue c (i : RunQueueId) =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for q in ctx.Dbo.RunQueue do
                    where (q.RunQueueId = i.value)
                    select (Some (q.WorkerNodeRunModelData, q.RunQueueStatusId))
                    exactlyOneOrDefault
                }

            match x with
            | Some (v, s) ->
                let w() =
                    try
                        match RunQueueStatus.tryCreate s with
                        | Some st -> (v |> deserialize serializationFormat, st) |> Ok
                        | None -> toError (fun x -> TryLoadRunQueueErr (x, $"Invalid status: {s}")) i
                    with
                    | e -> e |> DbExn |> DbErr |> Error

                tryRopFun (fun e -> e |> DbExn |> DbErr) w
            | None -> toError (fun x -> TryLoadRunQueueErr (x, "Unable to find run queue")) i

        tryDbFun g


    let loadAllActiveRunQueueId c =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for q in ctx.Dbo.RunQueue do
                    where (q.RunQueueStatusId =
                        RunQueueStatus.NotStartedRunQueue.value
                        || q.RunQueueStatusId = RunQueueStatus.InProgressRunQueue.value
                        || q.RunQueueStatusId = RunQueueStatus.CancelRequestedRunQueue.value)
                    select q.RunQueueId
                }

            x
            |> Seq.toList
            |> List.map RunQueueId
            |> Ok

        tryDbFun g


    let private addRunQueueRow (ctx : WorkerNodeDbContext) (w : WorkerNodeRunModelData) =
        let row = ctx.Dbo.RunQueue.Create(
                            RunQueueId = w.runningProcessData.runQueueId.value,
                            WorkerNodeRunModelData = (w |> serialize serializationFormat),
                            RunQueueStatusId = RunQueueStatus.NotStartedRunQueue.value,
                            CreatedOn = DateTime.Now,
                            ModifiedOn = DateTime.Now)

        row


    let saveRunQueue c (w : WorkerNodeRunModelData) =
        let g() =
            let ctx = getDbContext c
            let row = addRunQueueRow ctx w
            ctx.SubmitUpdates()

            Ok()

        tryDbFun g


    /// Can transition to InProgress from NotStarted or InProgress (to restart).
    /// If run queue is in RunQueueStatus_CancelRequested state then we don't allow restarting it automatically.
    /// This could happen when cancel requested has propagated to the database but the system then crashed and
    /// did not actually cancel the solver.
    let tryStartRunQueue c (q : RunQueueId) (ProcessId pid) =
        let g() =
            let ctx = getDbContext c
            let r = ctx.Procedures.TryStartRunQueue.Invoke(``@runQueueId`` = q.value, ``@processId`` = pid)
            r.ResultSet |> bindIntScalar TryStartRunQueueErr q

        tryDbFun g


    /// Can transition to Completed from InProgress or CancelRequested.
    let tryCompleteRunQueue c (q : RunQueueId) =
        let g() =
            let ctx = getDbContext c
            let r = ctx.Procedures.TryCompleteRunQueue.Invoke(``@runQueueId`` = q.value)
            r.ResultSet |> bindIntScalar TryCompleteRunQueueErr q

        tryDbFun g


    /// Can transition to Cancelled from NotStarted, InProgress, or CancelRequested.
    let tryCancelRunQueue c (q : RunQueueId) (errMsg : string) =
        let g() =
            let ctx = getDbContext c
            let r = ctx.Procedures.TryCancelRunQueue.Invoke(``@runQueueId`` = q.value, ``@errorMessage`` = errMsg)
            r.ResultSet |> bindIntScalar TryCancelRunQueueErr q

        tryDbFun g


    /// Can transition to Failed from InProgress or CancelRequested.
    let tryFailRunQueue c (q : RunQueueId) (errMsg : string) =
        let g() =
            let ctx = getDbContext c
            let r = ctx.Procedures.TryFailRunQueue.Invoke(``@runQueueId`` = q.value, ``@errorMessage`` = errMsg)
            r.ResultSet |> bindIntScalar TryFailRunQueueErr q

        tryDbFun g


    /// Can transition to CancelRequested from InProgress.
    let tryRequestCancelRunQueue c (q : RunQueueId) (r : CancellationType) =
        let g() =
            let ctx = getDbContext c
            let r = ctx.Procedures.TryRequestCancelRunQueue.Invoke(``@runQueueId`` = q.value, ``@notificationTypeId`` = r.value)
            r.ResultSet |> bindIntScalar TryRequestCancelRunQueueErr q

        tryDbFun g


    /// Can request notification of results when state is InProgress or CancelRequested.
    let tryNotifyRunQueue c (q : RunQueueId) (r : ResultNotificationType option) =
        let g() =
            let ctx = getDbContext c
            let v = r |> Option.bind (fun e -> Some e.value) |> Option.defaultValue 0
            let r = ctx.Procedures.TryNotifyRunQueue.Invoke(``@runQueueId`` = q.value, ``@notificationTypeId`` = v)
            r.ResultSet |> bindIntScalar TryNotifyRunQueueErr q

        tryDbFun g


    let tryCheckCancellation c (q : RunQueueId) =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for x in ctx.Dbo.RunQueue do
                    where (x.RunQueueId = q.value && x.RunQueueStatusId = RunQueueStatus.CancelRequestedRunQueue.value)
                    select (Some x.NotificationTypeId)
                    exactlyOneOrDefault
                }

            match x with
            | Some v ->
                match v with
                | 0 -> AbortCalculation None
                | _ -> CancelWithResults None
                |> Some
            | None -> None
            |> Ok

        tryDbFun g


    /// Check for notification only when InProgress
    let tryCheckNotification c (q : RunQueueId) =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for x in ctx.Dbo.RunQueue do
                    where (x.RunQueueId = q.value && x.RunQueueStatusId = RunQueueStatus.InProgressRunQueue.value)
                    select (Some x.NotificationTypeId)
                    exactlyOneOrDefault
                }

            match x with
            | Some v ->
                match v with
                | 0 -> None
                | 1 -> Some RegularChartGeneration
                | 2 -> Some ForceChartGeneration
                | _ -> None
                |> Ok
            | None -> TryCheckNotificationErr q |> DbErr |> Error

        tryDbFun g


    /// Clear notification only when InProgress
    let tryClearNotification c (q : RunQueueId) =
        let g() =
            let ctx = getDbContext c
            let r = ctx.Procedures.TryClearNotificationRunQueue.Invoke(``@runQueueId`` = q.value)
            r.ResultSet |> bindIntScalar TryClearNotificationErr q

        tryDbFun g


    let deleteRunQueue c (q : RunQueueId) =
        let g() =
            let ctx = getDbContext c
            let r = ctx.Procedures.DeleteRunQueue.Invoke(``@runQueueId`` = q.value)
            r.ResultSet |> bindIntScalar DeleteRunQueueEntryErr q

        tryDbFun g


    /// Can modify progress related information when state is InProgress or CancelRequested.
    let tryUpdateProgress c (q : RunQueueId) (td : ClmProgressData) =
        let g() =
            printfn $"tryUpdateProgress: RunQueueId: {q}, progress data: %A{td}."
            let ctx = getDbContext c
            let ee = td.eeData

            let r = ctx.Procedures.TryUpdateProgressRunQueue.Invoke(
                                        ``@runQueueId`` = q.value,
                                        ``@progress`` = td.progressData.progress,
                                        ``@callCount`` = td.progressData.callCount,
                                        ``@relativeInvariant`` = td.yRelative
                                        ``@maxEe`` = ee.maxEe,
                                        ``@maxAverageEe`` = ee.maxAverageEe,
                                        ``@maxWeightedAverageAbsEe`` = ee.maxWeightedAverageAbsEe,
                                        ``@maxLastEe`` = ee.maxLastEe)

            r.ResultSet |> bindIntScalar TryUpdateProgressRunQueueErr q

        tryDbFun g
