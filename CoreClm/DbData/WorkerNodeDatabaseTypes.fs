namespace DbData

open FSharp.Data
open System
open FSharp.Data.Sql
open System.Data.SQLite
open Dapper
open System.Data.Common

open Softellect.Sys.Primitives
open Softellect.Sys.Core
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
open MessagingServiceInfo.ServiceInfo

// ! Must be the last to open !
open Configuration

module WorkerNodeDatabaseTypes =

    let serializationFormat = BinaryZippedFormat

    type WorkerNodeDB = SqlProgrammabilityProvider<WorkerNodeSqlProviderName, ConfigFile = WorkerNodeAppConfigFile>
    type RunQueueTable = WorkerNodeDB.dbo.Tables.RunQueue
    type RunQueueTableRow = RunQueueTable.Row

    /// SQL to load / upsert RunQueue.
    type RunQueueTableData = SqlCommandProvider<"
        select *
        from dbo.RunQueue
        where runQueueId = @runQueueId", WorkerNodeConnectionStringValue, ResultType.DataReader>


    /// SQL to load first not started RunQueue.
    type RunQueueFirstTableData = SqlCommandProvider<"
        select top 1
            r.*
        from dbo.RunQueue r
        where r.runQueueStatusId = 0
        order by runQueueOrder", WorkerNodeConnectionStringValue, ResultType.DataReader>


    type WorkerNodeRunModelData
        with
//        static member tryCreate (r : RunQueueTableRow) : ClmResult<WorkerNodeRunModelData> =
//            let w() =
//                try
//                    r.workerNodeRunModelData |> deserialize serializationFormat
//                with
//                | e -> failwith ""
//
//            tryRopFun (fun e -> e |> GeneralFileExn |> FileErr) w

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

    type ProcessId =
        | ProcessId of int64


    /// kk:20210215 - The allowed transitions for RunQueue are as follows:
    ///     1. RunQueue starts with runQueueStatusId = 0 (NotStarted) and processId = null. The value of
    ///        startedOn should be null at this point but this is irrelevant as it is not used.
    ///     2. When a WorkerNode decides to spawn a SolverRunner (and it looks that for the time being
    ///        spawning instead of using threads is much better on 64+ core machines and under NET5) then it
    ///        transitions runQueueStatusId from 0 to 2 (InProgress). The value of processId is still null at this point
    ///        because SolverRunner has not started yet. The value of startedOn is irrelevant but it it set at this point.
    ///     3. SolverRunner should attempt to set processId to its process id (if processId is still null) and if that fails
    ///        then bail out and don't run.
    ///     4. WorkerNode at restart (or WorkerNodeAdm if asked at any time) may examine running processes and
    ///        if it is found that a SolverRunner with the appropriate RunQueueId is not running, then it may clear
    ///        the value of processId
    ///     5. If cancel request is received, then WorkerNode should get processId from DB and if it is not null, then
    ///        try killing the process with this id, provided that it is running SolverRunner with that RunQueueId.
    ///        Regardless the presence of such SolverRunner, RunQueue with that id should be cancelled and its
    ///        runQueueStatusId should be transitioned to 6 (Cancelled).
    ///     6. When SolverRunner completed successfully, it should transition runQueueStatusId from 2 to 3 (Completed).
    ///     7. And if SolverRunner fails (and catches the error), then it should transition runQueueStatusId from 2 to 4 (Failed).
    let tryStartRunQueue c (q : RunQueueId) (ProcessId pid) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString

            use cmd = new SqlCommandProvider<"
                update dbo.RunQueue
                set
                    processId = @processId,
                    runQueueStatusId = 2,
                    startedOn = (getdate()),
                    modifiedOn = (getdate())
                where runQueueId = @runQueueId and processId is null and runQueueStatusId = 0
            ", WorkerNodeConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

            let result = cmd.Execute(runQueueId = q.value, processId = pid)

            match result = 1 with
            | true -> Ok ()
            | false -> toError TryStartRunQueueErr q

        tryDbFun g
