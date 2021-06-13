namespace DbData

open Newtonsoft.Json
open FSharp.Data
open System

open Softellect.Sys
open Softellect.Sys.Core
open Softellect.Sys.MessagingPrimitives

open Clm.Substances
open Clm.ModelParams
open Clm.CalculationData
open Clm.ReactionRates
open DynamicSql
open ClmSys.GeneralErrors
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.WorkerNodeData
open ClmSys.PartitionerData

// ! Must be the last to open !
open Configuration

module DatabaseTypes =

    type ClmDB = SqlProgrammabilityProvider<ContGenSqlProviderName, ConfigFile = AppConfigFile>


    type ClmDefaultValueTable = ClmDB.dbo.Tables.ClmDefaultValue
    type ClmDefaultValueTableRow = ClmDefaultValueTable.Row


    type ClmDefaultValueData = SqlCommandProvider<"
        select *
        from dbo.ClmDefaultValue
        where clmDefaultValueId = @clmDefaultValueId", ContGenConnectionStringValue, ResultType.DataReader>


    type ClmTaskTable = ClmDB.dbo.Tables.ClmTask
    type ClmTaskTableRow = ClmTaskTable.Row


    type ClmTaskData = SqlCommandProvider<"
        select *
        from dbo.ClmTask
        where clmTaskId = @clmTaskId", ContGenConnectionStringValue, ResultType.DataReader>


    type ClmTaskByDefaultData = SqlCommandProvider<"
        select top 1 *
        from dbo.ClmTask
        where clmDefaultValueId = @clmDefaultValueId
        order by createdOn", ContGenConnectionStringValue, ResultType.DataReader>


    type ClmTaskAllIncompleteData = SqlCommandProvider<"
        select *
        from dbo.ClmTask
        where remainingRepetitions > 0 and clmTaskStatusId = 0
        order by clmTaskOrder", ContGenConnectionStringValue, ResultType.DataReader>


    type CommandLineParamTable = ClmDB.dbo.Tables.CommandLineParam
    type CommandLineParamTableRow = CommandLineParamTable.Row


    type CommandLineParamData = SqlCommandProvider<"
        select *
        from dbo.CommandLineParam
        where clmTaskId = @clmTaskId
        order by createdOn", ContGenConnectionStringValue, ResultType.DataReader>


    type ModelDataTable = ClmDB.dbo.Tables.ModelData
    type ModelDataTableRow = ModelDataTable.Row


    type ModelDataTableData = SqlCommandProvider<"
        select
            modelDataId,
            clmTaskId,
            seedValue,
            modelDataParams,
            modelBinaryData,
            createdOn
        from dbo.ModelData
        where modelDataId = @modelDataId", ContGenConnectionStringValue, ResultType.DataReader>


    type RunQueueTable = ClmDB.dbo.Tables.RunQueue
    type RunQueueTableRow = RunQueueTable.Row


    /// SQL to upsert RunQueue.
    type RunQueueTableData = SqlCommandProvider<"
        select *
        from dbo.RunQueue
        where runQueueId = @runQueueId", ContGenConnectionStringValue, ResultType.DataReader>


    /// SQL to load all not started RunQueue.
    type RunQueueAllTableData = SqlCommandProvider<"
        select
            r.*,
            t.clmDefaultValueId
        from dbo.RunQueue r
        inner join dbo.ModelData m on r.modelDataId = m.modelDataId
        inner join dbo.ClmTask t on m.clmTaskId = t.clmTaskId
        where r.runQueueStatusId = 0 and r.progress = 0 and t.clmTaskStatusId = 0 and r.workerNodeId is null
        order by runQueueOrder", ContGenConnectionStringValue, ResultType.DataReader>


    /// SQL to load RunQueue by runQueueId.
    type RunQueueSingleTableData = SqlCommandProvider<"
        select
            r.*,
            t.clmDefaultValueId
        from dbo.RunQueue r
        inner join dbo.ModelData m on r.modelDataId = m.modelDataId
        inner join dbo.ClmTask t on m.clmTaskId = t.clmTaskId
        where runQueueId = @runQueueId", ContGenConnectionStringValue, ResultType.DataReader>


    /// SQL to load first not started RunQueue.
    type RunQueueFirstTableData = SqlCommandProvider<"
        select top 1
            r.*,
            t.clmDefaultValueId
        from dbo.RunQueue r
        inner join dbo.ModelData m on r.modelDataId = m.modelDataId
        inner join dbo.ClmTask t on m.clmTaskId = t.clmTaskId
        where r.runQueueStatusId = 0 and r.progress = 0 and t.clmTaskStatusId = 0 and r.workerNodeId is null
        order by runQueueOrder", ContGenConnectionStringValue, ResultType.DataReader>


    /// SQL to load all currently running models == total progress.
    /// runQueueStatusId = 2 is InProgressRunQueue.
    type RunQueueProgressTableData = SqlCommandProvider<"
        select
            r.*,
            t.clmDefaultValueId
        from dbo.RunQueue r
        inner join dbo.ModelData m on r.modelDataId = m.modelDataId
        inner join dbo.ClmTask t on m.clmTaskId = t.clmTaskId
        where r.runQueueStatusId = 2 and t.clmTaskStatusId = 0 and r.workerNodeId is not null
        order by runQueueOrder", ContGenConnectionStringValue, ResultType.DataReader>


    type WorkerNodeTable = ClmDB.dbo.Tables.WorkerNode
    type WorkerNodeTableRow = WorkerNodeTable.Row


    type WorkerNodeTableData = SqlCommandProvider<"
        select *
        from dbo.WorkerNode
        where workerNodeId = @workerNodeId", ContGenConnectionStringValue, ResultType.DataReader>


    type ClmDefaultValue
        with
        static member create (r : ClmDefaultValueTableRow) =
            {
                clmDefaultValueId = r.clmDefaultValueId |> ClmDefaultValueId
                defaultRateParams = r.defaultRateParams |> JsonConvert.DeserializeObject<ReactionRateProviderParams>
                description = r.description
            }


    type ModelCommandLineParam
        with

        static member create (r : CommandLineParamTableRow) =
            {
                y0 = r.y0
                tEnd = r.tEnd
                useAbundant = r.useAbundant
            }

        /// TODO kk:20190531 - Perhaps it is worth assigning commandLineParamId on the client OR by database.
        member r.addRow (ClmTaskId clmTaskId) (t : CommandLineParamTable) =
            let newRow =
                t.NewRow(
                        commandLineParamId = Guid.NewGuid(),
                        clmTaskId = clmTaskId,
                        y0 = r.y0,
                        tEnd = r.tEnd,
                        useAbundant = r.useAbundant
                        )

            t.Rows.Add newRow
            newRow


    type ClmTask
        with

        static member tryCreate c (r : ClmTaskTableRow) =
            match r.numberOfAminoAcids |> NumberOfAminoAcids.tryCreate, r.maxPeptideLength |> MaxPeptideLength.tryCreate with
            | Some n, Some m ->
                let clmTaskId = r.clmTaskId |> ClmTaskId

                match c clmTaskId with
                | Ok p ->
                    {
                        clmTaskInfo =
                            {
                                clmTaskId = clmTaskId
                                clmDefaultValueId = r.clmDefaultValueId |> ClmDefaultValueId
                                numberOfAminoAcids = n
                                maxPeptideLength = m
                            }
                        commandLineParams = p
                        numberOfRepetitions = r.numberOfRepetitions
                        remainingRepetitions = r.remainingRepetitions
                        createdOn = r.createdOn
                    }
                    |> Ok
                | Error e -> addError ClmTaskTryCreatErr r.clmTaskId e
            | _ -> toError ClmTaskTryCreatErr r.clmTaskId

        member r.addRow (t : ClmTaskTable) =
            let newRow =
                t.NewRow(
                        clmTaskId = r.clmTaskInfo.clmTaskId.value,
                        clmDefaultValueId = r.clmTaskInfo.clmDefaultValueId.value,
                        numberOfAminoAcids = r.clmTaskInfo.numberOfAminoAcids.length,
                        maxPeptideLength = r.clmTaskInfo.maxPeptideLength.length,
                        numberOfRepetitions = r.numberOfRepetitions,
                        remainingRepetitions = r.remainingRepetitions,
                        createdOn = DateTime.Now
                        )

            t.Rows.Add newRow
            newRow


    type ModelData
        with

        static member tryCreate (c : ClmTaskId -> Result<ClmTask, ClmError>) (r : ModelDataTableRow) =
            match r.clmTaskId |> ClmTaskId |> c with
            | Ok i ->
                let rawData =
                    {
                        seedValue = r.seedValue
                        modelData =
                            {
                                modelDataParams = r.modelDataParams |> JsonConvert.DeserializeObject<ModelDataParams>
                                modelBinaryData = r.modelBinaryData |> unZip |> JsonConvert.DeserializeObject<ModelBinaryData>
                            }
                    }

                {
                    modelDataId = r.modelDataId |> ModelDataId
                    clmTaskInfo = i.clmTaskInfo
                    data = rawData
                }
                |> Ok
            | Error e ->  addError ModelDataTryCreateErr r.modelDataId e


    type RunQueue
        with

        static member tryCreate d (r : RunQueueTableRow) =
            match RunQueueStatus.tryCreate r.runQueueStatusId with
            | Some s ->
                {
                    runQueueId = RunQueueId r.runQueueId
                    info =
                        {
                            modelDataId = ModelDataId r.modelDataId
                            defaultValueId = d

                            modelCommandLineParam =
                                {
                                    y0 = r.y0
                                    tEnd = r.tEnd
                                    useAbundant = r.useAbundant
                                }
                        }
                    runQueueStatus = s
                    workerNodeIdOpt = r.workerNodeId |> Option.bind (fun e -> e |> MessagingClientId |> WorkerNodeId |> Some)

                    progressData =
                        {
                            progress = r.progress
                            callCount = r.callCount
                            yRelative = r.yRelative

                            eeData =
                                {
                                    maxEe = r.maxEe
                                    maxAverageEe = r.maxAverageEe
                                    maxWeightedAverageAbsEe = r.maxWeightedAverageAbsEe
                                    maxLastEe = r.maxLastEe
                                }

                            errorMessageOpt = r.errorMessage |> Option.map ErrorMessage
                        }

                    createdOn = r.createdOn
                }
                |> Some
            | None -> None

        member r.addRow (t : RunQueueTable) =
            let newRow =
                t.NewRow(
                        runQueueId = r.runQueueId.value,
                        modelDataId = r.info.modelDataId.value,
                        runQueueStatusId = r.runQueueStatus.value,
                        y0 = r.modelCommandLineParam.y0,
                        tEnd = r.modelCommandLineParam.tEnd,
                        useAbundant = r.modelCommandLineParam.useAbundant,
                        progress = r.progressData.progress,
                        callCount = r.progressData.callCount,
                        yRelative = r.progressData.yRelative,
                        maxEe = r.progressData.eeData.maxEe,
                        maxAverageEe = r.progressData.eeData.maxAverageEe,
                        maxWeightedAverageAbsEe = r.progressData.eeData.maxWeightedAverageAbsEe,
                        maxLastEe = r.progressData.eeData.maxLastEe,
                        workerNodeId = (r.workerNodeIdOpt |> Option.bind (fun e -> Some e.value.value)),
                        modifiedOn = DateTime.Now
                        )

            newRow.errorMessage <- r.progressData.errorMessageOpt |> Option.bind (fun e -> Some e.value)
            t.Rows.Add newRow
            newRow

        /// The following transitions are allowed here:
        ///
        ///     NotStartedRunQueue + None (workerNodeId) -> RunRequestedRunQueue + Some workerNodeId - scheduled (but not yet confirmed) new work.
        ///     NotStartedRunQueue + None (workerNodeId) -> CancelledRunQueue + None (workerNodeId) - cancelled work that has not been scheduled yet.
        ///
        ///     RunRequestedRunQueue + Some workerNodeId -> NotStartedRunQueue + None (workerNodeId) - the node rejected work.
        ///     RunRequestedRunQueue + Some workerNodeId -> InProgressRunQueue + the same Some workerNodeId - the node accepted work.
        ///     RunRequestedRunQueue + Some workerNodeId -> CancelRequestedRunQueue + the same Some workerNodeId -
        //          scheduled (but not yet confirmed) new work, which then was requested to be cancelled before the node replied.
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
        member q.tryUpdateRow (r : RunQueueTableRow) =
            let toError e = e |> RunQueueTryUpdateRowErr |> DbErr |> Error

            let g s u =
                r.runQueueId <- q.runQueueId.value
                r.workerNodeId <- (q.workerNodeIdOpt |> Option.bind (fun e -> Some e.value.value))
                r.progress <- q.progressData.progress
                r.callCount <- q.progressData.callCount
                r.yRelative <- q.progressData.yRelative
                r.maxEe <- q.progressData.eeData.maxEe
                r.maxAverageEe <- q.progressData.eeData.maxAverageEe
                r.maxAverageEe <- q.progressData.eeData.maxAverageEe
                r.maxWeightedAverageAbsEe <- q.progressData.eeData.maxWeightedAverageAbsEe
                r.maxLastEe <- q.progressData.eeData.maxLastEe
                r.errorMessage <- q.progressData.errorMessageOpt |> Option.bind (fun e -> Some e.value)

                match s with
                | Some (Some v) -> r.startedOn <- Some v
                | Some None-> r.startedOn <- None
                | None -> ()

                r.modifiedOn <- DateTime.Now

                match u with
                | true -> r.runQueueStatusId <- q.runQueueStatus.value
                | false -> ()

                Ok()

            let f s =
                {
                    runQueueId = q.runQueueId
                    runQueueStatusFrom = s
                    runQueueStatusTo = q.runQueueStatus
                    workerNodeIdOptFrom = r.workerNodeId |> Option.bind (fun e -> e |> MessagingClientId |> WorkerNodeId |> Some)
                    workerNodeIdOptTo = q.workerNodeIdOpt
                }

            let f1 e = e |> InvalidStatusTransitionErr |> toError
            let f2 e = e |> InvalidDataErr |> toError

            match RunQueueStatus.tryCreate r.runQueueStatusId with
            | Some s ->
                match s, r.workerNodeId, q.runQueueStatus, q.workerNodeIdOpt with
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


    type WorkerNodeInfo
        with

        /// TODO kk:20200329 - Note that partitionerId is hard coded. Revisit if necessary.
        static member create (r : WorkerNodeTableRow) =
            {
                workerNodeId = r.workerNodeId |> MessagingClientId |> WorkerNodeId
                workerNodeName = r.workerNodeName |> WorkerNodeName
                noOfCores = r.numberOfCores
                partitionerId = defaultPartitionerId
                nodePriority = r.nodePriority |> WorkerNodePriority
                isInactive = r.isInactive
                lastErrorDateOpt = r.lastErrorOn
            }

        member w.addRow (t : WorkerNodeTable) =
            let newRow =
                t.NewRow(
                        workerNodeId = w.workerNodeId.value.value,
                        workerNodeName = w.workerNodeName.value,
                        numberOfCores = w.noOfCores,
                        nodePriority = w.nodePriority.value,
                        lastErrorOn = w.lastErrorDateOpt
                        )

            newRow.modifiedOn <- DateTime.Now
            t.Rows.Add newRow
            newRow

        member w.updateRow (r : WorkerNodeTableRow) =
            r.workerNodeName <- w.workerNodeName.value
            r.numberOfCores <- w.noOfCores
            r.nodePriority <- w.nodePriority.value
            r.modifiedOn <- DateTime.Now
            r.lastErrorOn <- w.lastErrorDateOpt


    let loadClmDefaultValue c (ClmDefaultValueId clmDefaultValueId) =
        let g() =
            use conn = getOpenConn c
            use d = new ClmDefaultValueData(conn)
            let t = new ClmDefaultValueTable()
            d.Execute clmDefaultValueId |> t.Load

            t.Rows
            |> Seq.tryFind (fun e -> e.clmDefaultValueId = clmDefaultValueId)
            |> Option.bind (fun v -> ClmDefaultValue.create v |> Some)
            |> mapDbError LoadClmDefaultValueErr clmDefaultValueId

        tryDbFun g


    let upsertClmDefaultValue c (p : ClmDefaultValue) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString

            use cmd = new SqlCommandProvider<"
                merge ClmDefaultValue as target
                using (select @clmDefaultValueId, @defaultRateParams, @description) as source (clmDefaultValueId, defaultRateParams, description)
                on (target.clmDefaultValueId = source.clmDefaultValueId)
                when not matched then
                    insert (clmDefaultValueId, defaultRateParams, description)
                    values (source.clmDefaultValueId, source.defaultRateParams, source.description)
                when matched then
                    update set defaultRateParams = source.defaultRateParams, description = source.description;
            ", ContGenConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

            let result = cmd.Execute(clmDefaultValueId = p.clmDefaultValueId.value
                                    , defaultRateParams = (p.defaultRateParams |> JsonConvert.SerializeObject)
                                    , description = match p.description with | Some d -> d | None -> null)

            match result = 1 with
            | true -> Ok ()
            | false -> toError UpsertClmDefaultValueErr p.clmDefaultValueId.value

        tryDbFun g


    let loadCommandLineParams c (ClmTaskId clmTaskId) =
        let g() =
            use conn = getOpenConn c
            use d = new CommandLineParamData(conn)
            let t = new CommandLineParamTable()
            d.Execute clmTaskId |> t.Load

            t.Rows
            |> Seq.toList
            |> List.map (fun r -> ModelCommandLineParam.create r)
            |> Ok

        tryDbFun g


    let addCommandLineParams c clmTaskId (p : ModelCommandLineParam) =
        let g() =
            use conn = getOpenConn c
            use t = new CommandLineParamTable()
            p.addRow clmTaskId t |> ignore
            t.Update conn |> ignore
            Ok ()

        tryDbFun g


    let loadClmTask c (ClmTaskId clmTaskId) =
        let g() =
            use conn = getOpenConn c
            use d = new ClmTaskData(conn)
            let t = new ClmTaskTable()
            d.Execute clmTaskId |> t.Load

            match t.Rows |> Seq.tryFind (fun e -> e.clmTaskId = clmTaskId) with
            | Some v -> ClmTask.tryCreate (loadCommandLineParams c) v
            | None -> toError LoadClmTaskErr clmTaskId

        tryDbFun g


    let loadClmTaskByDefault c (ClmDefaultValueId clmDefaultValueId) =
        let g() =
            use conn = getOpenConn c
            use d = new ClmTaskByDefaultData(conn)
            let t = new ClmTaskTable()
            d.Execute clmDefaultValueId |> t.Load

            match t.Rows |> Seq.tryFind (fun e -> e.clmDefaultValueId = clmDefaultValueId) with
            | Some v -> ClmTask.tryCreate (loadCommandLineParams c) v
            | None -> toError LoadClmTaskByDefaultErr clmDefaultValueId

        tryDbFun g


    let loadIncompleteClmTasks c =
        let g() =
            use conn = getOpenConn c
            use d = new ClmTaskAllIncompleteData(conn)
            let t = new ClmTaskTable()
            d.Execute() |> t.Load

            t.Rows
            |> Seq.toList
            |> List.map (fun r -> ClmTask.tryCreate (loadCommandLineParams c) r)
            |> Ok

        tryDbFun g


    let addClmTask c (clmTask : ClmTask) =
        let g() =
            use conn = getOpenConn c
            use t = new ClmTaskTable()
            clmTask.addRow t |> ignore
            t.Update conn |> ignore

            clmTask.commandLineParams
            |> List.map (addCommandLineParams c clmTask.clmTaskInfo.clmTaskId)
            |> ignore

            Ok()

        tryDbFun g


    /// Updates remainingRepetitions of ClmTask.
    let updateClmTask c (clmTask : ClmTask) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString

            use cmd = new SqlCommandProvider<"
                    UPDATE dbo.ClmTask
                    SET remainingRepetitions = @remainingRepetitions
                    WHERE clmTaskId = @clmTaskId
                ", ContGenConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

            let recordsUpdated =
                cmd.Execute(
                    clmTaskId = clmTask.clmTaskInfo.clmTaskId.value,
                    remainingRepetitions = clmTask.remainingRepetitions)

            match recordsUpdated = 1 with
            | true -> Ok ()
            | false -> toError UpdateClmTaskErr clmTask.clmTaskInfo.clmTaskId.value

        tryDbFun g


    let loadModelData c (ModelDataId modelDataId) =
        let g() =
            use conn = getOpenConn c
            use d = new ModelDataTableData(conn)
            let t = new ModelDataTable()
            d.Execute modelDataId |> t.Load

            match t.Rows |> Seq.tryFind (fun e -> e.modelDataId = modelDataId) with
            | Some v -> ModelData.tryCreate (loadClmTask c) v
            | None -> toError LoadModelDataError modelDataId

        tryDbFun g


    let upsertModelData c (m : ModelData) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString

            let recordsUpdated =
                use cmdWithBinaryData = new SqlCommandProvider<"
                    merge ModelData as target
                    using (select @modelDataId, @clmTaskId, @seedValue, @modelDataParams, @modelBinaryData, @createdOn)
                    as source (modelDataId, clmTaskId, seedValue, modelDataParams, modelBinaryData, createdOn)
                    on (target.modelDataId = source.modelDataId)
                    when not matched then
                        insert (modelDataId, clmTaskId, seedValue, modelDataParams, modelBinaryData, createdOn)
                        values (source.modelDataId, source.clmTaskId, source.seedValue, source.modelDataParams, source.modelBinaryData, source.createdOn)
                    when matched then
                        update set clmTaskId = source.clmTaskId, seedValue = source.seedValue, modelDataParams = source.modelDataParams, modelBinaryData = source.modelBinaryData, createdOn = source.createdOn;
                    ", ContGenConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

                cmdWithBinaryData.Execute(
                    modelDataId = m.modelDataId.value,
                    clmTaskId = m.clmTaskInfo.clmTaskId.value,
                    seedValue = (match m.data.seedValue with | Some s -> s | None -> -1),
                    modelDataParams = (m.data.modelData.modelDataParams |> JsonConvert.SerializeObject),
                    modelBinaryData = (m.data.modelData.modelBinaryData |> JsonConvert.SerializeObject |> zip),
                    createdOn = DateTime.Now)

            match recordsUpdated = 1 with
            | true -> Ok ()
            | false -> toError UpdateModelDataErr m.modelDataId.value

        tryDbFun g


//    /// Do not delete. It shows how to use OUTPUT clause.
//    let saveResultDataOld c (r : ResultDataWithId) =
//        let g() =
//            use conn = getOpenConn c
//            let connectionString = conn.ConnectionString
//
//            use cmd = new SqlCommandProvider<"
//                INSERT INTO dbo.ResultData
//                            (resultDataId
//                            ,modelDataId
//                            ,workerNodeId
//                            ,y0
//                            ,tEnd
//                            ,useAbundant
//                            ,maxEe
//                            ,maxAverageEe
//                            ,maxWeightedAverageAbsEe
//                            ,maxLastEe
//                            ,createdOn)
//                        OUTPUT Inserted.resultDataId
//                        VALUES
//                            (@resultDataId
//                            ,@modelDataId
//                            ,@workerNodeId
//                            ,@y0
//                            ,@tEnd
//                            ,@useAbundant
//                            ,@maxEe
//                            ,@maxAverageEe
//                            ,@maxWeightedAverageAbsEe
//                            ,@maxLastEe
//                            ,@createdOn)
//            ", ContGenConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
//
//            let result =
//                cmd.Execute(
//                        resultDataId = r.resultDataId.value
//                        ,modelDataId = r.resultData.modelDataId.value
//                        ,workerNodeId = r.workerNodeId.messagingClientId.value
//                        ,y0 = r.resultData.y0
//                        ,tEnd = r.resultData.tEnd
//                        ,useAbundant = r.resultData.useAbundant
//                        ,maxEe = r.resultData.maxEe
//                        ,maxAverageEe = r.resultData.maxAverageEe
//                        ,maxWeightedAverageAbsEe = r.resultData.maxWeightedAverageAbsEe
//                        ,maxLastEe = r.resultData.maxLastEe
//                        ,createdOn = DateTime.Now)
//                |> Seq.toList
//
//            match result.Length = 1 with
//            | true -> Ok ()
//            | false -> toError SaveResultDataErr r.resultDataId.value
//
//        tryDbFun g

//    let saveResultData c (r : ResultDataWithId) =
//        let g() =
//            use conn = getOpenConn c
//            let connectionString = conn.ConnectionString
//
//            use cmd = new SqlCommandProvider<"
//                merge ResultData as target
//                using (
//                    select
//                        @resultDataId
//                        ,@modelDataId
//                        ,@workerNodeId
//                        ,@y0
//                        ,@tEnd
//                        ,@useAbundant
//                        ,@maxEe
//                        ,@maxAverageEe
//                        ,@maxWeightedAverageAbsEe
//                        ,@maxLastEe
//                        ,@createdOn)
//                    as source
//                        (resultDataId
//                        ,modelDataId
//                        ,workerNodeId
//                        ,y0
//                        ,tEnd
//                        ,useAbundant
//                        ,maxEe
//                        ,maxAverageEe
//                        ,maxWeightedAverageAbsEe
//                        ,maxLastEe
//                        ,createdOn)
//                on (target.resultDataId = source.resultDataId)
//                when not matched then
//                    insert
//                        (resultDataId
//                        ,modelDataId
//                        ,workerNodeId
//                        ,y0
//                        ,tEnd
//                        ,useAbundant
//                        ,maxEe
//                        ,maxAverageEe
//                        ,maxWeightedAverageAbsEe
//                        ,maxLastEe
//                        ,createdOn)
//                    values
//                        (source.resultDataId
//                        ,source.modelDataId
//                        ,source.workerNodeId
//                        ,source.y0
//                        ,source.tEnd
//                        ,source.useAbundant
//                        ,source.maxEe
//                        ,source.maxAverageEe
//                        ,source.maxWeightedAverageAbsEe
//                        ,source.maxLastEe
//                        ,source.createdOn)
//                when matched then
//                    update
//                    set
//                        modelDataId = source.modelDataId
//                        ,workerNodeId = source.workerNodeId
//                        ,y0 = source.y0
//                        ,tEnd = source.tEnd
//                        ,useAbundant = source.useAbundant
//                        ,maxEe = source.maxEe
//                        ,maxAverageEe = source.maxAverageEe
//                        ,maxWeightedAverageAbsEe = source.maxWeightedAverageAbsEe
//                        ,maxLastEe = source.maxLastEe
//                        ,createdOn = source.createdOn;
//            ", ContGenConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)
//
//            let result =
//                cmd.Execute(
//                        resultDataId = r.resultDataId.value
//                        ,modelDataId = r.resultData.modelDataId.value
//                        ,workerNodeId = r.workerNodeId.messagingClientId.value
//                        ,y0 = r.resultData.y0
//                        ,tEnd = r.resultData.tEnd
//                        ,useAbundant = r.resultData.useAbundant
//                        ,maxEe = r.resultData.maxEe
//                        ,maxAverageEe = r.resultData.maxAverageEe
//                        ,maxWeightedAverageAbsEe = r.resultData.maxWeightedAverageAbsEe
//                        ,maxLastEe = r.resultData.maxLastEe
//                        ,createdOn = DateTime.Now)
//
//            match result = 1 with
//            | true -> Ok ()
//            | false -> toError SaveResultDataErr r.resultDataId.value
//
//        tryDbFun g


//    let tryLoadResultData c (ResultDataId resultDataId) =
//        let g() =
//            use conn = getOpenConn c
//            use d = new ResultDataTableData(conn)
//            let t = new ResultDataTable()
//            d.Execute(resultDataId = resultDataId) |> t.Load
//
//            t.Rows
//            |> Seq.tryFind (fun e -> e.resultDataId = resultDataId)
//            |> Option.bind (fun v -> ResultDataWithId.create v |> Some)
//            |> Ok
//
//        tryDbFun g


    let private mapRunQueue (reader : DynamicSqlDataReader) =
        match RunQueueStatus.tryCreate reader?runQueueStatusId with
        | Some s ->
            {
                runQueueId = RunQueueId reader?runQueueId

                info =
                    {
                        modelDataId = ModelDataId reader?modelDataId
                        defaultValueId = ClmDefaultValueId reader?clmDefaultValueId

                        modelCommandLineParam =
                            {
                                y0 = reader?y0
                                tEnd = reader?tEnd
                                useAbundant = reader?useAbundant
                            }
                    }

                runQueueStatus = s
                workerNodeIdOpt = reader?workerNodeId |> Option.bind (fun e -> e |> MessagingClientId |> WorkerNodeId |> Some)

                progressData =
                    {
                        progress = reader?progress
                        callCount = reader?callCount
                        yRelative = reader?yRelative

                        eeData =
                            {
                                maxEe = reader?maxEe
                                maxAverageEe = reader?maxAverageEe
                                maxWeightedAverageAbsEe = reader?maxWeightedAverageAbsEe
                                maxLastEe = reader?maxLastEe
                            }

                        errorMessageOpt = reader?errorMessage |> Option.map ErrorMessage
                    }

                createdOn = reader?createdOn
            }
            |> Ok
        | None -> toError MapRunQueueErr (reader?runQueueId)


    let loadRunQueue c =
        let g() =
            seq
                {
                    use conn = getOpenConn c
                    use data = new RunQueueAllTableData(conn)
                    use reader= new DynamicSqlDataReader(data.Execute())
                    while (reader.Read()) do yield mapRunQueue reader
                        }
            |> List.ofSeq
            |> Ok

        tryDbFun g


    let loadRunQueueProgress c =
        let g() =
            seq
                {
                    use conn = getOpenConn c
                    use data = new RunQueueProgressTableData(conn)
                    use reader= new DynamicSqlDataReader(data.Execute())
                    while (reader.Read()) do yield mapRunQueue reader
                        }
            |> List.ofSeq
            |> Ok

        tryDbFun g


    let tryLoadFirstRunQueue c =
        let g() =
            seq
                {
                    use conn = getOpenConn c
                    use data = new RunQueueFirstTableData(conn)
                    use reader= new DynamicSqlDataReader(data.Execute())
                    while (reader.Read()) do yield mapRunQueue reader
                        }
            |> List.ofSeq
            |> List.tryHead
            |> Ok

        tryDbFun g |> Rop.unwrapResultOption


    let tryLoadRunQueue c (q : RunQueueId) =
        let g() =
            seq
                {
                    use conn = getOpenConn c
                    use data = new RunQueueSingleTableData(conn)
                    use reader= new DynamicSqlDataReader(data.Execute q.value)
                    while (reader.Read()) do yield mapRunQueue reader
                        }
            |> List.ofSeq
            |> List.tryHead
            |> Ok

        tryDbFun g |> Rop.unwrapResultOption


    let tryResetRunQueue c (q : RunQueueId) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString

            use cmd = new SqlCommandProvider<"update dbo.RunQueue set runQueueStatusId = 0, errorMessage = null, workerNodeId = null, startedOn = null, modifiedOn = getdate() where runQueueId = @runQueueId and runQueueStatusId = 4", ContGenConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

            match cmd.Execute(runQueueId = q.value) = 1 with
            | true -> Ok ()
            | false -> toError ResetRunQueueEntryErr q

        tryDbFun g


    let saveRunQueue c modelDataId defaultValueId p =
        let g() =
            use conn = getOpenConn c
            use t = new RunQueueTable()
            let r = RunQueue.fromModelCommandLineParam modelDataId defaultValueId p
            let row = r.addRow t
            t.Update conn |> ignore
            row.runQueueId |> RunQueueId |> Ok

        tryDbFun g


    let deleteRunQueue c (runQueueId : RunQueueId) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString

            use cmd = new SqlCommandProvider<"delete from dbo.RunQueue where runQueueId = @runQueueId", ContGenConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

            match cmd.Execute(runQueueId = runQueueId.value) = 1 with
            | true -> Ok ()
            | false -> toError DeleteRunQueueEntryErr runQueueId

        tryDbFun g


    let upsertRunQueue c (w : RunQueue) =
        let g() =
            use conn = getOpenConn c
            use d = new RunQueueTableData(conn)
            let t = new RunQueueTable()
            d.Execute w.runQueueId.value |> t.Load

            let result =
                match t.Rows |> Seq.tryFind (fun e -> e.runQueueId = w.runQueueId.value) with
                | Some r -> w.tryUpdateRow r
                | None -> w.addRow t |> ignore; Ok()
                |> Rop.bind (fun () -> t.Update conn |> ignore; Ok())

            result

        tryDbFun g


    let loadWorkerNodeInfo c (i : WorkerNodeId) =
        let g() =
            use conn = getOpenConn c
            use d = new WorkerNodeTableData(conn)
            let t = new WorkerNodeTable()
            d.Execute i.value.value |> t.Load

            match t.Rows |> Seq.tryFind (fun e -> e.workerNodeId = i.value.value) with
            | Some v -> v |> WorkerNodeInfo.create |> Ok
            | None -> toError LoadWorkerNodeInfoErr i.value.value

        tryDbFun g


    let upsertWorkerNodeInfo c (w : WorkerNodeInfo) =
        let g() =
            use conn = getOpenConn c
            use d = new WorkerNodeTableData(conn)
            let t = new WorkerNodeTable()
            d.Execute w.workerNodeId.value.value |> t.Load

            match t.Rows |> Seq.tryFind (fun e -> e.workerNodeId = w.workerNodeId.value.value) with
            | Some r -> w.updateRow r
            | None -> w.addRow t |> ignore

            t.Update conn |> ignore
            Ok()

        tryDbFun g


    let upsertWorkerNodeErr c i =
        let g() =
            match loadWorkerNodeInfo c i with
            | Ok w -> upsertWorkerNodeInfo c { w with lastErrorDateOpt = Some DateTime.Now }
            | Error e -> Error e

        tryDbFun g


    /// SQL to get the first available worker node to schedule work.
    [<Literal>]
    let AvailableWorkerNodeSql = @"
        ; with q as
        (
        select
            workerNodeId
            ,nodePriority
            ,cast(
                case
                    when numberOfCores <= 0 then 1
                    else (select count(1) as runningModels from RunQueue where workerNodeId = w.workerNodeId and runQueueStatusId in (2, 5, 7)) / (cast(numberOfCores as money))
                end as money) as workLoad
            ,case when lastErrorOn is null or dateadd(minute, @lastAllowedNodeErrInMinutes, lastErrorOn) < getdate() then 0 else 1 end as noErr
        from WorkerNode w
        where isInactive = 0
        )
        select top 1
        workerNodeId
        from q
        where noErr = 0 and workLoad < 1
        order by nodePriority desc, workLoad, newid()"


    type AvailableWorkerNodeTableData = SqlCommandProvider<AvailableWorkerNodeSql, ContGenConnectionStringValue, ResultType.DataReader>


    let tryGetAvailableWorkerNode c (LastAllowedNodeErr m) =
        let g() =
            use conn = getOpenConn c
            use cmd = new SqlCommandProvider<AvailableWorkerNodeSql, ContGenConnectionStringValue, ResultType.DataTable>(conn)
            let table = cmd.Execute (m / 1<minute>)

            match table.Rows |> Seq.tryHead with
            | None -> Ok None
            | Some r -> r.workerNodeId |> MessagingClientId |> WorkerNodeId |> Some |> Ok

        tryDbFun g
