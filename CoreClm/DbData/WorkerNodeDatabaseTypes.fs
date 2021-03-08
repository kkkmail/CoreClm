namespace DbData

open ClmSys.SolverData
open ClmSys.WorkerNodeData
open FSharp.Data
open System
open FSharp.Data.Sql
open DynamicSql
open Softellect.Sys
open Softellect.Sys.Core
open Softellect.Sys.Primitives
open Softellect.Sys.Retry
open ClmSys.GeneralErrors
open ClmSys.ClmErrors
open ClmSys.GeneralPrimitives
open ClmSys.ContGenPrimitives
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


    type WorkerNodeDB = SqlProgrammabilityProvider<WorkerNodeSqlProviderName, ConfigFile = AppConfigFile>
    type RunQueueTable = WorkerNodeDB.dbo.Tables.RunQueue
    type RunQueueTableRow = RunQueueTable.Row

    /// SQL to load / upsert RunQueue.
    type RunQueueTableData = SqlCommandProvider<"
        select *
        from dbo.RunQueue
        where runQueueId = @runQueueId", WorkerNodeConnectionStringValue, ResultType.DataReader>


    /// SQL to load first not started runQueueId.
    [<Literal>]
    let runQueueFirstSql = "
        select top 1 runQueueId
        from dbo.RunQueue
        where runQueueStatusId = " + RunQueueStatus_NotStarted + "
        order by runQueueOrder"


    type RunQueueFirstTableData =
        SqlCommandProvider<runQueueFirstSql, WorkerNodeConnectionStringValue, ResultType.DataReader>


    /// Sql to load count of all running solvers.
    [<Literal>]
    let runningSolversCountSql = "
        select isnull(count(*), 0) as runCount
        from dbo.RunQueue
        where runQueueStatusId = " + RunQueueStatus_InProgress


    type RunningSolversCountTableData =
        SqlCommandProvider<runningSolversCountSql, WorkerNodeConnectionStringValue, ResultType.DataReader>


    /// Sql to load all running solvers.
    [<Literal>]
    let runningSolversSql = "
        select
            runQueueId,
            processId
        from dbo.RunQueue
        where runQueueStatusId in (" + RunQueueStatus_InProgress + ", " + RunQueueStatus_CancelRequested + ")"


    type RunningSolversTableData =
        SqlCommandProvider<runningSolversSql, WorkerNodeConnectionStringValue, ResultType.DataReader>


    let mapSolverRunnerInfo (reader : DynamicSqlDataReader) =
        {
            runQueueId = reader?runQueueId |> RunQueueId
            processId = reader?processId  |> Option.bind (fun e -> e |> ProcessId |> Some)
        }


    let tryLoadSolverRunners c =
        let g() =
            seq
                {
                    use conn = getOpenConn c
                    use data = new RunningSolversTableData(conn)
                    use reader= new DynamicSqlDataReader(data.Execute())
                    while (reader.Read()) do yield mapSolverRunnerInfo reader
                        }
            |> List.ofSeq
            |> Ok

        tryDbFun g


    let private mapSolverRunnersCount (reader : DynamicSqlDataReader) =
        int reader?runCount |> Ok


    let tryGetRunningSolversCount c =
        let g() =
            seq
                {
                    use conn = getOpenConn c
                    use data = new RunningSolversCountTableData(conn)
                    use reader= new DynamicSqlDataReader(data.Execute())
                    while (reader.Read()) do yield mapSolverRunnersCount reader
                        }
            |> List.ofSeq
            |> List.tryHead
            |> Ok

        tryDbFun g |> Rop.unwrapResultOption


    type WorkerNodeRunModelData
        with
        static member tryCreate (r : RunQueueTableRow) : ClmResult<WorkerNodeRunModelData> =
            let w() =
                try
                    r.workerNodeRunModelData |> deserialize serializationFormat |> Ok
                with
                | e -> e |> DbExn |> DbErr |> Error

            tryRopFun (fun e -> e |> DbExn |> DbErr) w

        member r.addRow(t : RunQueueTable) =
            let newRow =
                    t.NewRow(
                            runQueueId = r.runningProcessData.runQueueId.value,
                            workerNodeRunModelData = (r |> serialize serializationFormat),
                            runQueueStatusId = RunQueueStatus.NotStartedRunQueue.value,
                            createdOn = DateTime.Now,
                            modifiedOn = DateTime.Now
                            )

            t.Rows.Add newRow
            newRow


    let private mapRunQueueId (reader : DynamicSqlDataReader) =
        RunQueueId reader?runQueueId |> Ok


    let tryPickRunQueue c =
        let g() =
            seq
                {
                    use conn = getOpenConn c
                    use data = new RunQueueFirstTableData(conn)
                    use reader= new DynamicSqlDataReader(data.Execute())
                    while (reader.Read()) do yield mapRunQueueId reader
                        }
            |> List.ofSeq
            |> List.tryHead
            |> Ok

        tryDbFun g |> Rop.unwrapResultOption


    let tryLoadRunQueue c (q : RunQueueId) =
        let g() =
            use conn = getOpenConn c
            use d = new RunQueueTableData(conn)
            let t = new RunQueueTable()
            d.Execute q.value |> t.Load

            match t.Rows |> Seq.tryFind (fun e -> e.runQueueId = q.value) with
            | Some v -> v |> WorkerNodeRunModelData.tryCreate
            | None -> toError TryLoadRunQueueErr q

        tryDbFun g


    /// Sql to load incomplete run queue.
    [<Literal>]
    let incompleteRunQueueSql = "
        select
            runQueueId
        from dbo.RunQueue
        where runQueueStatusId in (" + RunQueueStatus_NotStarted + ", " + RunQueueStatus_InProgress + ", " + RunQueueStatus_CancelRequested + ")"


    type IncompleteRunQueueTableData =
        SqlCommandProvider<incompleteRunQueueSql, WorkerNodeConnectionStringValue, ResultType.DataReader>


    let mapIncompleteRunQueue (reader : DynamicSqlDataReader) =
        reader?runQueueId |> RunQueueId


    let loadAllActiveRunQueueId c =
        let g() =
            seq
                {
                    use conn = getOpenConn c
                    use data = new IncompleteRunQueueTableData(conn)
                    use reader= new DynamicSqlDataReader(data.Execute())
                    while (reader.Read()) do yield mapIncompleteRunQueue reader
                        }
            |> List.ofSeq
            |> Ok

        tryDbFun g


    let saveRunQueue c (w : WorkerNodeRunModelData) =
        let g() =
            use conn = getOpenConn c
            use t = new RunQueueTable()
            let row = w.addRow t
            t.Update conn |> ignore
            Ok()

        tryDbFun g


    /// Can transition to InProgress from NotStarted or InProgress (to restart).
    /// If run queue is in RunQueueStatus_CancelRequested state then we don't allow restarting it automatically.
    /// This could happen when cancel requested has propagated to the database but the system then crashed and
    /// did not actually cancel the solver.
    [<Literal>]
    let tryStartRunQueueSql = "
        update dbo.RunQueue
        set
            processId = @processId,
            runQueueStatusId = " + RunQueueStatus_InProgress + ",
            startedOn = (getdate()),
            modifiedOn = (getdate())
        where runQueueId = @runQueueId and runQueueStatusId in (" + RunQueueStatus_NotStarted + ", " + RunQueueStatus_InProgress + ")"


    let tryStartRunQueue c (q : RunQueueId) (ProcessId pid) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString
            use cmd = new SqlCommandProvider<tryStartRunQueueSql, WorkerNodeConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
            cmd.Execute(runQueueId = q.value, processId = pid) |> bindError TryStartRunQueueErr q

        tryDbFun g


    /// Can transition to Completed from InProgress or CancelRequested.
    [<Literal>]
    let tryCompleteRunQueueSql = "
        update dbo.RunQueue
        set
            runQueueStatusId = " + RunQueueStatus_Completed + ",
            processId = null,
            modifiedOn = (getdate())
        where runQueueId = @runQueueId and processId is not null and runQueueStatusId in (" + RunQueueStatus_InProgress + ", " + RunQueueStatus_CancelRequested + ")"


    let tryCompleteRunQueue c (q : RunQueueId) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString
            use cmd = new SqlCommandProvider<tryCompleteRunQueueSql, WorkerNodeConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
            cmd.Execute(runQueueId = q.value) |> bindError TryCompleteRunQueueErr q

        tryDbFun g


    /// Can transition to Cancelled from NotStarted, InProgress, or CancelRequested.
    [<Literal>]
    let tryCancelRunQueueSql = "
        update dbo.RunQueue
        set
            runQueueStatusId = " + RunQueueStatus_Cancelled + ",
            processId = null,
            modifiedOn = (getdate()),
            errorMessage = @errorMessage
        where runQueueId = @runQueueId and runQueueStatusId in (" + RunQueueStatus_NotStarted + ", " + RunQueueStatus_InProgress + ", " + RunQueueStatus_CancelRequested + ")"


    let tryCancelRunQueue c (q : RunQueueId) (errMsg : string) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString
            use cmd = new SqlCommandProvider<tryCancelRunQueueSql, WorkerNodeConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
            cmd.Execute(runQueueId = q.value, errorMessage = errMsg) |> bindError TryCancelRunQueueErr q

        tryDbFun g


    /// Can transition to Failed from InProgress or CancelRequested.
    [<Literal>]
    let tryFailRunQueueSql = "
        update dbo.RunQueue
        set
            runQueueStatusId = " + RunQueueStatus_Failed + ",
            processId = null,
            modifiedOn = (getdate()),
            errorMessage = @errorMessage
        where runQueueId = @runQueueId and runQueueStatusId in (" + RunQueueStatus_InProgress + ", " + RunQueueStatus_CancelRequested + ")"


    let tryFailRunQueue c (q : RunQueueId) (errMsg : string) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString
            use cmd = new SqlCommandProvider<tryFailRunQueueSql, WorkerNodeConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
            cmd.Execute(runQueueId = q.value, errorMessage = errMsg) |> bindError TryFailRunQueueErr q

        tryDbFun g


    /// !!! kk:20210217 - WTF !!!  - Putting the first " on the same line as tryRequestCancelRunQueueSql breaks compilation but it is the same as above !!!
    /// Can transition to CancelRequested from InProgress.
    [<Literal>]
    let tryRequestCancelRunQueueSql =
        "
        update dbo.RunQueue
        set
            runQueueStatusId = " + RunQueueStatus_CancelRequested + ",
            notificationTypeId = @notificationTypeId,
            modifiedOn = (getdate())
        where runQueueId = @runQueueId and runQueueStatusId = " + RunQueueStatus_InProgress


    let tryRequestCancelRunQueue c (q : RunQueueId) (r : CancellationType) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString
            use cmd = new SqlCommandProvider<tryRequestCancelRunQueueSql, WorkerNodeConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
            cmd.Execute(runQueueId = q.value, notificationTypeId = r.value) |> bindError TryRequestCancelRunQueueErr q

        tryDbFun g


    /// Can request notification of results when state is InProgress or CancelRequested.
    [<Literal>]
    let tryNotifySql = "
        update dbo.RunQueue
        set
            notificationTypeId = @notificationTypeId,
            modifiedOn = (getdate())
        where runQueueId = @runQueueId and runQueueStatusId in (" + RunQueueStatus_InProgress + ", " + RunQueueStatus_CancelRequested + ")"


    let tryNotifyRunQueue c (q : RunQueueId) (r : ResultNotificationType option) =
        let g() =
            let v = r |> Option.bind (fun e -> Some e.value) |> Option.defaultValue 0
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString
            use cmd = new SqlCommandProvider<tryNotifySql, WorkerNodeConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
            cmd.Execute(runQueueId = q.value, notificationTypeId = v) |> bindError TryNotifyRunQueueErr q

        tryDbFun g


    /// Can modify progress when state is InProgress or CancelRequested.
    [<Literal>]
    let tryUpdateProgressSql = "
        update dbo.RunQueue
        set
            progress = @progress,
            modifiedOn = (getdate())
        where runQueueId = @runQueueId and runQueueStatusId in (" + RunQueueStatus_InProgress + ", " + RunQueueStatus_CancelRequested + ")"


    let tryUpdateProgressRunQueue c (q : RunQueueId) (p : TaskProgress) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString
            use cmd = new SqlCommandProvider<tryUpdateProgressSql, WorkerNodeConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
            cmd.Execute(runQueueId = q.value, progress = p.value) |> bindError TryUpdateProgressRunQueueErr q

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
    [<Literal>]
    let tryClearNotificationQueueSql =
        "
        update dbo.RunQueue
        set
            notificationTypeId = 0,
            modifiedOn = (getdate())
        where runQueueId = @runQueueId and runQueueStatusId in (" + RunQueueStatus_InProgress + ")"


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


    /// Can modify time related information when state is InProgress or CancelRequested.
    [<Literal>]
    let tryUpdateTimeSql = "
        update dbo.RunQueue
        set
            progressDetailed = @progressDetailed,
            callCount = @callCount,
            y = @y,
            maxEe = @maxEe,
            maxAverageEe = @maxAverageEe,
            maxWeightedAverageAbsEe = @maxWeightedAverageAbsEe,
            maxLastEe = @maxLastEe,
            modifiedOn = (getdate())
        where runQueueId = @runQueueId and runQueueStatusId in (" + RunQueueStatus_InProgress + ", " + RunQueueStatus_CancelRequested + ")"


    let tryUpdateTime c (q : RunQueueId) (td : TimeData) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString
            use cmd = new SqlCommandProvider<tryUpdateTimeSql, WorkerNodeConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
            let ee = td.eeData
            cmd.Execute(runQueueId = q.value, progressDetailed = td.progressDetailed, callCount = td.callCount, y = td.y, maxEe = ee.maxEe, maxAverageEe = ee.maxAverageEe, maxWeightedAverageAbsEe = ee.maxWeightedAverageAbsEe, maxLastEe = ee.maxLastEe)
            |> bindError TryUpdateProgressRunQueueErr q

        tryDbFun g
