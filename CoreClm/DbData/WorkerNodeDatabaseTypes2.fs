namespace DbData

open ClmSys.WorkerNodeData
open FSharp.Data
open System
open FSharp.Data.Sql
//open DynamicSql
open Softellect.Sys
open Softellect.Sys.Core
open Softellect.Sys.Primitives
open Softellect.Sys.Retry
open ClmSys.GeneralErrors
open ClmSys.ClmErrors
open ClmSys.GeneralPrimitives
open ClmSys.SolverRunnerPrimitives
open MessagingServiceInfo.ServiceInfo
open ClmSys.WorkerNodeErrors

// ! Must be the last to open !
open Configuration

module WorkerNodeDatabaseTypes =

    let serializationFormat = BinaryZippedFormat


    /// Binds an unsuccessful database update operation to a given continuation function.
    let private bindError f q r =
        match r = 1 with
        | true -> Ok ()
        | false -> toError f q


    //type WorkerNodeDB = SqlProgrammabilityProvider<WorkerNodeSqlProviderName, ConfigFile = AppConfigFile>

    type private WorkerNodeDb = SqlDataProvider<
                    Common.DatabaseProviderTypes.MSSQLSERVER,
                    ConnectionString = WorkerNodeConnectionStringValue,
                    UseOptionTypes = true>


    type private WorkerNodeDbContext = WorkerNodeDb.dataContext
    let private getDbContext (c : unit -> ConnectionString) = c().value |> WorkerNodeDb.GetDataContext


    type private RunQueueEntity = WorkerNodeDbContext.``dbo.RunQueueEntity``
    type private MessageEntity = WorkerNodeDbContext.``dbo.MessageEntity``


    ///// SQL to load / upsert RunQueue.
    //type RunQueueTableData = SqlCommandProvider<"
    //    select *
    //    from dbo.RunQueue
    //    where runQueueId = @runQueueId", WorkerNodeConnectionStringValue, ResultType.DataReader>


    ///// SQL to load first not started runQueueId.
    //[<Literal>]
    //let runQueueFirstSql = "
    //    select top 1 runQueueId
    //    from dbo.RunQueue
    //    where runQueueStatusId = " + RunQueueStatus_NotStarted + "
    //    order by runQueueOrder"


    //type RunQueueFirstTableData =
    //    SqlCommandProvider<runQueueFirstSql, WorkerNodeConnectionStringValue, ResultType.DataReader>



    let tryLoadSolverRunners c =
        let g() =
            let ctx = getDbContext c

            let x = 
                query {
                    for q in ctx.Dbo.RunQueue do
                    where (q.RunQueueStatusId = RunQueueStatus_InProgress || q.RunQueueStatusId = RunQueueStatus_CancelRequested)
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
                    where (q.RunQueueStatusId = RunQueueStatus_InProgress)
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
                    where (q.RunQueueStatusId = RunQueueStatus_NotStarted)
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
                    select (Some q.WorkerNodeRunModelData)
                    exactlyOneOrDefault
                }

            match x with
            | Some v ->
                let w() =
                    try
                        v |> deserialize serializationFormat |> Ok
                    with
                    | e -> e |> DbExn |> DbErr |> Error

                tryRopFun (fun e -> e |> DbExn |> DbErr) w
            | None -> toError TryLoadRunQueueErr i

        tryDbFun g


    let loadAllActiveRunQueueId c =
        let g() =
            let ctx = getDbContext c

            let x =
                query {
                    for q in ctx.Dbo.RunQueue do
                    where (q.RunQueueStatusId = RunQueueStatus_NotStarted || q.RunQueueStatusId = RunQueueStatus_InProgress || q.RunQueueStatusId = RunQueueStatus_CancelRequested)
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
    //[<Literal>]
    //let tryStartRunQueueSql = "
    //    update dbo.RunQueue
    //    set
    //        processId = @processId,
    //        runQueueStatusId = " + RunQueueStatus_InProgress + ",
    //        startedOn = (getdate()),
    //        modifiedOn = (getdate())
    //    where runQueueId = @runQueueId and runQueueStatusId in (" + RunQueueStatus_NotStarted + ", " + RunQueueStatus_InProgress + ")"


    let tryStartRunQueue c (q : RunQueueId) (ProcessId pid) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString
            use cmd = new SqlCommandProvider<tryStartRunQueueSql, WorkerNodeConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
            cmd.Execute(runQueueId = q.value, processId = pid) |> bindError TryStartRunQueueErr q

        tryDbFun g


    /// Can transition to Completed from InProgress or CancelRequested.
    //[<Literal>]
    //let tryCompleteRunQueueSql = "
    //    update dbo.RunQueue
    //    set
    //        runQueueStatusId = " + RunQueueStatus_Completed + ",
    //        processId = null,
    //        modifiedOn = (getdate())
    //    where runQueueId = @runQueueId and processId is not null and runQueueStatusId in (" + RunQueueStatus_InProgress + ", " + RunQueueStatus_CancelRequested + ")"


    let tryCompleteRunQueue c (q : RunQueueId) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString
            use cmd = new SqlCommandProvider<tryCompleteRunQueueSql, WorkerNodeConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
            cmd.Execute(runQueueId = q.value) |> bindError TryCompleteRunQueueErr q

        tryDbFun g


    /// Can transition to Cancelled from NotStarted, InProgress, or CancelRequested.
    //[<Literal>]
    //let tryCancelRunQueueSql = "
    //    update dbo.RunQueue
    //    set
    //        runQueueStatusId = " + RunQueueStatus_Cancelled + ",
    //        processId = null,
    //        modifiedOn = (getdate()),
    //        errorMessage = @errorMessage
    //    where runQueueId = @runQueueId and runQueueStatusId in (" + RunQueueStatus_NotStarted + ", " + RunQueueStatus_InProgress + ", " + RunQueueStatus_CancelRequested + ")"


    let tryCancelRunQueue c (q : RunQueueId) (errMsg : string) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString
            use cmd = new SqlCommandProvider<tryCancelRunQueueSql, WorkerNodeConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
            cmd.Execute(runQueueId = q.value, errorMessage = errMsg) |> bindError TryCancelRunQueueErr q

        tryDbFun g


    /// Can transition to Failed from InProgress or CancelRequested.
    //[<Literal>]
    //let tryFailRunQueueSql = "
    //    update dbo.RunQueue
    //    set
    //        runQueueStatusId = " + RunQueueStatus_Failed + ",
    //        processId = null,
    //        modifiedOn = (getdate()),
    //        errorMessage = @errorMessage
    //    where runQueueId = @runQueueId and runQueueStatusId in (" + RunQueueStatus_InProgress + ", " + RunQueueStatus_CancelRequested + ")"


    let tryFailRunQueue c (q : RunQueueId) (errMsg : string) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString
            use cmd = new SqlCommandProvider<tryFailRunQueueSql, WorkerNodeConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
            cmd.Execute(runQueueId = q.value, errorMessage = errMsg) |> bindError TryFailRunQueueErr q

        tryDbFun g


    /// !!! kk:20210217 - WTF !!!  - Putting the first " on the same line as tryRequestCancelRunQueueSql breaks compilation but it is the same as above !!!
    /// Can transition to CancelRequested from InProgress.
    //[<Literal>]
    //let tryRequestCancelRunQueueSql =
    //    "
    //    update dbo.RunQueue
    //    set
    //        runQueueStatusId = " + RunQueueStatus_CancelRequested + ",
    //        notificationTypeId = @notificationTypeId,
    //        modifiedOn = (getdate())
    //    where runQueueId = @runQueueId and runQueueStatusId = " + RunQueueStatus_InProgress


    let tryRequestCancelRunQueue c (q : RunQueueId) (r : CancellationType) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString
            use cmd = new SqlCommandProvider<tryRequestCancelRunQueueSql, WorkerNodeConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
            cmd.Execute(runQueueId = q.value, notificationTypeId = r.value) |> bindError TryRequestCancelRunQueueErr q

        tryDbFun g


    /// Can request notification of results when state is InProgress or CancelRequested.
    //[<Literal>]
    //let tryNotifySql = "
    //    update dbo.RunQueue
    //    set
    //        notificationTypeId = @notificationTypeId,
    //        modifiedOn = (getdate())
    //    where runQueueId = @runQueueId and runQueueStatusId in (" + RunQueueStatus_InProgress + ", " + RunQueueStatus_CancelRequested + ")"


    let tryNotifyRunQueue c (q : RunQueueId) (r : ResultNotificationType option) =
        let g() =
            let v = r |> Option.bind (fun e -> Some e.value) |> Option.defaultValue 0
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString
            use cmd = new SqlCommandProvider<tryNotifySql, WorkerNodeConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
            cmd.Execute(runQueueId = q.value, notificationTypeId = v) |> bindError TryNotifyRunQueueErr q

        tryDbFun g


    [<Literal>]
    let tryCheckCancellationSql = "
        select
            notificationTypeId
        from dbo.RunQueue
        where runQueueId = @runQueueId and runQueueStatusId = " + RunQueueStatus_CancelRequested


    type CheckCancellationTableData =
        SqlCommandProvider<tryCheckCancellationSql, WorkerNodeConnectionStringValue, ResultType.DataReader>


    let private mapCheckCancellation (reader : DynamicSqlDataReader) =
        match int reader?notificationTypeId with
        | 0 -> AbortCalculation None
        | _ -> CancelWithResults None
        |> Ok


    let tryCheckCancellation c (RunQueueId q) =
        let g() =
            seq
                {
                    use conn = getOpenConn c
                    use data = new CheckCancellationTableData(conn)
                    use reader = new DynamicSqlDataReader(data.Execute(runQueueId = q))
                    while (reader.Read()) do yield mapCheckCancellation reader
                        }
            |> List.ofSeq
            |> List.tryHead
            |> Ok

        tryDbFun g |> Rop.unwrapResultOption


    /// Check for notification only when InProgress
    [<Literal>]
    let tryCheckNotificationSql = "
        select
            notificationTypeId
        from dbo.RunQueue
        where runQueueId = @runQueueId and runQueueStatusId in (" + RunQueueStatus_InProgress + ")"


    type CheckNotificationTableData =
        SqlCommandProvider<tryCheckNotificationSql, WorkerNodeConnectionStringValue, ResultType.DataReader>


    let private mapCheckNotification (reader : DynamicSqlDataReader) =
        match int reader?notificationTypeId with
        | 0 -> None
        | 1 -> Some RegularChartGeneration
        | 2 -> Some ForceChartGeneration
        | _ -> None


    let tryCheckNotification c (RunQueueId q) =
        let g() =
            seq
                {
                    use conn = getOpenConn c
                    use data = new CheckNotificationTableData(conn)
                    use reader = new DynamicSqlDataReader(data.Execute(runQueueId = q))
                    while (reader.Read()) do yield mapCheckNotification reader
                        }
            |> List.ofSeq
            |> List.tryHead
            |> Option.bind id
            |> Ok

        tryDbFun g


    /// Clear notification only when InProgress
    //[<Literal>]
    //let tryClearNotificationQueueSql =
    //    "
    //    update dbo.RunQueue
    //    set
    //        notificationTypeId = 0,
    //        modifiedOn = (getdate())
    //    where runQueueId = @runQueueId and runQueueStatusId in (" + RunQueueStatus_InProgress + ")"


    let tryClearNotification c (q : RunQueueId) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString
            use cmd = new SqlCommandProvider<tryClearNotificationQueueSql, WorkerNodeConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
            cmd.Execute(runQueueId = q.value) |> bindError TryClearNotificationErr q

        tryDbFun g


    let deleteRunQueue c (q : RunQueueId) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString
            use cmd = new SqlCommandProvider<"delete from dbo.RunQueue where runQueueId = @runQueueId", WorkerNodeConnectionStringValue>(connectionString)

            match cmd.Execute(runQueueId = q.value) with
            | 0 | 1 -> Ok()
            | _ -> q |> CannotDeleteRunQueueErr |> OnRunModelErr |> WorkerNodeErr |> Error

        tryDbFun g


    /// Can modify progress related information when state is InProgress or CancelRequested.
    //[<Literal>]
    //let tryUpdateProgressSql = "
    //    update dbo.RunQueue
    //    set
    //        progress = @progress,
    //        callCount = @callCount,
    //        yRelative = @yRelative,
    //        maxEe = @maxEe,
    //        maxAverageEe = @maxAverageEe,
    //        maxWeightedAverageAbsEe = @maxWeightedAverageAbsEe,
    //        maxLastEe = @maxLastEe,
    //        modifiedOn = (getdate())
    //    where runQueueId = @runQueueId and runQueueStatusId in (" + RunQueueStatus_InProgress + ", " + RunQueueStatus_CancelRequested + ")"


    let tryUpdateProgress c (q : RunQueueId) (td : ProgressData) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString
            use cmd = new SqlCommandProvider<tryUpdateProgressSql, WorkerNodeConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
            let ee = td.eeData

            cmd.Execute(
                    runQueueId = q.value,
                    progress = td.progress,
                    callCount = td.callCount,
                    yRelative = td.yRelative,
                    maxEe = ee.maxEe,
                    maxAverageEe = ee.maxAverageEe,
                    maxWeightedAverageAbsEe = ee.maxWeightedAverageAbsEe,
                    maxLastEe = ee.maxLastEe)

            |> bindError TryUpdateProgressRunQueueErr q

        tryDbFun g
