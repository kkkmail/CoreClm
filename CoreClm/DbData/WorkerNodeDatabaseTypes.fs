namespace DbData

open ClmSys.WorkerNodeData
open FSharp.Data
open System
open FSharp.Data.Sql
open System.Data.SQLite
open Dapper
open System.Data.Common
open DynamicSql

open Softellect.Sys
open Softellect.Sys.Core
open Softellect.Sys.Primitives
open Softellect.Sys.Retry
open Softellect.Sys.MessagingPrimitives
open Softellect.Messaging.Primitives
open Softellect.Sys.MessagingClientErrors
open Softellect.Sys.MessagingServiceErrors

open ClmSys.VersionInfo
open ClmSys.GeneralErrors
open ClmSys.ClmErrors
open ClmSys.GeneralPrimitives
open Clm.ModelParams
open ClmSys.MessagingData
open ClmSys.SolverRunnerPrimitives
open MessagingServiceInfo.ServiceInfo

// ! Must be the last to open !
open Configuration

module WorkerNodeDatabaseTypes =

    let serializationFormat = BinaryZippedFormat


    /// Binds an unsuccessful database update operation to a given continuation function.
    let bindError f q r =
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
        where runQueueStatusId = " + RunQueueStatus_InProgress


    type RunningSolversTableData =
        SqlCommandProvider<runningSolversSql, WorkerNodeConnectionStringValue, ResultType.DataReader>


    let mapSolverRunnerInfo (reader : DynamicSqlDataReader) =
        {
            processId = reader?processId |> ProcessId
            runQueueId = reader?runQueueId |> RunQueueId
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
                    r.workerNodeRunModelData |> deserialize serializationFormat
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
            | None -> toError TryLoadRunQueue q

        tryDbFun g


    let saveRunQueue c (w : WorkerNodeRunModelData) =
        let g() =
            use conn = getOpenConn c
            use t = new RunQueueTable()
            let row = w.addRow t
            t.Update conn |> ignore
            row.runQueueId |> RunQueueId |> Ok

        tryDbFun g


    /// Can transition to InProgress from NotStarted.
    [<Literal>]
    let tryStartRunQueueSql = "
        update dbo.RunQueue
        set
            processId = @processId,
            runQueueStatusId = " + RunQueueStatus_InProgress + ",
            startedOn = (getdate()),
            modifiedOn = (getdate())
        where runQueueId = @runQueueId and processId is null and runQueueStatusId = " + RunQueueStatus_NotStarted


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
            modifiedOn = (getdate())
        where runQueueId = @runQueueId and runQueueStatusId = " + RunQueueStatus_InProgress


    let tryRequestCancelRunQueue c (q : RunQueueId) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString
            use cmd = new SqlCommandProvider<tryRequestCancelRunQueueSql, WorkerNodeConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
            cmd.Execute(runQueueId = q.value) |> bindError TryRequestCancelRunQueue q

        tryDbFun g
