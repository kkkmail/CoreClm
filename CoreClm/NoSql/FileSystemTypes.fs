namespace NoSql

open System
open System.IO

open Softellect.Sys.Primitives
open Softellect.Sys.Core
open Softellect.Sys.Retry
open Softellect.Sys.MessagingPrimitives
open Softellect.Messaging.Primitives

open ClmSys.GeneralData
open ClmSys.GeneralErrors
open Clm.ModelParams
open Clm.CalculationData
open MessagingServiceInfo.ServiceInfo
open ClmSys.WorkerNodeData
open ClmSys.MessagingPrimitives
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.SolverRunnerErrors

module FileSystemTypes =

    let serializationFormat = BinaryZippedFormat
    let serializationErrFormat = XmlFormat
    let private fileStorageFolder = DefaultFileStorageFolder


    type TableName =
        | TableName of string


    let messageTblName = TableName "Message"
    let modelDataTblName = TableName "ModelData"
    let resultDataTblName = TableName "ResultData"
    let chartInfoTblName = TableName "ChartInfo"
    let workerNodeRunModelDataTblName = TableName "WorkerNodeRunModelData"
    let workerNodeInfoTblName = TableName "WorkerNodeInfo"
    let runModelParamWithRemoteIdTblName = TableName "RunModelParamWithRemoteId"
    let workerNodeStateTblName = TableName "WorkerNodeState"
    let partitionerQueueElementTblName = TableName "PartitionerQueueElement"
    let solverRunnerErrTblName = TableName "SolverRunnerErr"


    let getFolderName (MessagingClientName serviceName) (TableName tableName) =
        let folder = fileStorageFolder + "\\" + serviceName + "\\" + tableName

        try
            Directory.CreateDirectory(folder) |> ignore
            Ok folder
        with
        | e -> e |> GetFolderNameExn |> FileErr |> Error


    let getFileName<'A> (fmt : SerializationFormat) serviceName tableName (objectId : 'A) =
        try
            match getFolderName serviceName tableName with
            | Ok folder ->
                let file = Path.Combine(folder, objectId.ToString() + "." + fmt.fileExtension)
                Ok file
            | Error e -> Error e
        with
        | e -> e |> GetFileNameExn |> FileErr |> Error


    /// Tries to load data.
    /// Returns (Ok (Some Object)) if object was found and successfully loaded.
    /// Returns (Ok None) if the object is not found.
    /// Returns (Error e) in case of any other issues.
    let tryLoadData<'T, 'A> serviceName tableName (objectId : 'A) : (Result<'T option, ClmError>) =
        let w () =
            try
                match getFileName serializationFormat serviceName tableName objectId with
                | Ok f ->
                    let x =
                        if File.Exists f
                        then
                            let data = File.ReadAllBytes (f)
                            let retVal = data |> deserialize serializationFormat |> Some |> Ok
                            retVal
                        else Ok None
                    x
                | Error e -> Error e
            with
            | e -> e |> ReadFileExn |> FileErr |> Error
        tryRopFun (fun e -> e |> GeneralFileExn |> FileErr) w


    /// Loads the data if successfull and returns an error if an object is not found OR any error occurs.
    let loadData<'T, 'A> serviceName tableName (objectId : 'A) : (Result<'T, ClmError>) =
        match tryLoadData<'T, 'A> serviceName tableName objectId with
        | Ok (Some r) -> Ok r
        | Ok None ->
            match getFileName<'A> serializationFormat serviceName tableName objectId with
            | Ok f -> f |> FileNotFoundErr |> FileErr |> Error
            | Error e -> Error e
        | Error e -> Error e


    let saveDataImpl<'T, 'A> fmt serviceName tableName (objectId : 'A) (t : 'T) =
        let w() =
            try
                match getFileName fmt serviceName tableName objectId with
                | Ok f ->
                    let d = t |> serialize fmt
                    File.WriteAllBytes (f, d)
                    Ok ()
                | Error e -> Error e
            with
            | e -> e |> WriteFileExn |> FileErr |> Error
        tryRopFun (fun e -> e |> GeneralFileExn |> FileErr) w


    let saveData<'T, 'A> serviceName tableName (objectId : 'A) (t : 'T) =
        saveDataImpl<'T, 'A> serializationFormat serviceName tableName objectId t


    /// Write-once error objects.
    let saveErrData<'T, 'A> serviceName tableName (objectId : 'A) (t : 'T) =
        saveDataImpl<'T, 'A> serializationErrFormat serviceName tableName objectId t


    /// Tries to delete object if it exists.
    let tryDeleteData<'T, 'A> serviceName tableName (objectId : 'A) =
        let w() =
            try
                match getFileName serializationFormat serviceName tableName objectId with
                | Ok f ->
                    if File.Exists f then File.Delete f
                    Ok ()
                | Error e -> Error e
            with
            | e -> e |> DeleteFileExn |> FileErr |> Error
        tryRopFun (fun e -> e |> GeneralFileExn |> FileErr) w


    let getObjectIds<'A> serviceName tableName (creator : string -> 'A) =
        let w() =
            try
                match getFolderName serviceName tableName with
                | Ok folder ->
                    Directory.GetFiles(folder, "*." + serializationFormat.fileExtension)
                    |> List.ofArray
                    |> List.map (fun e -> Path.GetFileNameWithoutExtension e)
                    |> List.map creator
                    |> Ok
                | Error e -> Error e
            with
            | e -> e |> GetObjectIdsExn |> FileErr |> Error
        tryRopFun (fun e -> e |> GeneralFileExn |> FileErr) w


    let loadObjects<'T, 'A> serviceName tableName (creator : string -> 'A) =
        match getObjectIds serviceName tableName creator with
        | Ok i ->
            i
            |> List.map (loadData<'T, 'A> serviceName tableName)
            |> Ok
        | Error e -> Error e


    //type TableActions<'T, 'A> =
    //    {
    //        save : 'T -> bool
    //        tryLoad : 'A -> 'T option
    //        tryDelete : 'A -> unit
    //        getIds : unit -> list<'A>
    //    }
    //
    //
    //let createTableActions<'T, 'A> tableName (getId : 'T -> 'A) (getObject : Guid -> 'A) serviceName =
    //    {
    //        save = fun m -> saveData<'T, 'A> serviceName tableName (getId m) m
    //        tryLoad = fun m -> tryLoadData<'T, 'A> serviceName tableName m
    //        tryDelete = fun m -> tryDeleteData<'T, 'A> serviceName tableName m
    //        getIds = fun () -> (getObjectIds<Guid> serviceName tableName Guid.Parse |> List.map getObject)
    //    }


    let saveMessageFs serviceName (m : Message) = saveData<Message, Guid> serviceName messageTblName m.messageDataInfo.messageId.value m
    let loadMessageFs serviceName (MessageId messageId) = loadData<Message, Guid> serviceName messageTblName messageId
    let tryDeleteMessageFs serviceName (MessageId messageId) = tryDeleteData<Message, Guid> serviceName messageTblName messageId
    let getMessageIdsFs serviceName () = getObjectIds<MessageId> serviceName messageTblName (fun e -> e |> Guid.Parse |> MessageId)
    let loadMessageAllFs serviceName () = loadObjects<Message, Guid> serviceName messageTblName Guid.Parse

    let saveModelDataFs serviceName (m : ModelData) = saveData<ModelData, Guid> serviceName modelDataTblName m.modelDataId.value m
    let loadModelDataFs serviceName (ModelDataId modelDataId) = loadData<ModelData, Guid> serviceName modelDataTblName modelDataId
    let tryDeleteModelDataFs serviceName (ModelDataId modelDataId) = tryDeleteData<ModelData, Guid> serviceName modelDataTblName modelDataId
    let getModelDataIdsFs serviceName () = getObjectIds<ModelDataId> serviceName modelDataTblName (fun e -> e |> Guid.Parse |> ModelDataId)
    let loadModelDataAllsFs serviceName () = loadObjects<ModelData, Guid> serviceName modelDataTblName Guid.Parse

    let saveWorkerNodeRunModelDataFs serviceName (m : WorkerNodeRunModelData) = saveData<WorkerNodeRunModelData, Guid> serviceName workerNodeRunModelDataTblName m.runningProcessData.runQueueId.value m
    let loadWorkerNodeRunModelDataFs serviceName (RunQueueId runQueueId) = loadData<WorkerNodeRunModelData, Guid> serviceName workerNodeRunModelDataTblName runQueueId
    let tryDeleteWorkerNodeRunModelDataFs serviceName (RunQueueId runQueueId) = tryDeleteData<WorkerNodeRunModelData, Guid> serviceName workerNodeRunModelDataTblName runQueueId
    let getWorkerNodeRunModelDataIdsFs serviceName () = getObjectIds<RunQueueId> serviceName workerNodeRunModelDataTblName (fun e -> e |> Guid.Parse |> RunQueueId)
    let loadWorkerNodeRunModelDataAllFs serviceName () = loadObjects<WorkerNodeRunModelData, Guid> serviceName workerNodeRunModelDataTblName Guid.Parse

    let saveResultDataFs serviceName (r : ResultDataWithId) = saveData<ResultDataWithId, Guid> serviceName resultDataTblName r.resultDataId.value r
    let loadResultDataFs serviceName (ResultDataId resultDataId) = loadData<ResultDataWithId, Guid> serviceName resultDataTblName resultDataId
    let tryDeleteResultDataFs serviceName (ResultDataId resultDataId) = tryDeleteData<ResultDataWithId, Guid> serviceName resultDataTblName resultDataId
    let getResultDataIdsFs serviceName () = getObjectIds<ResultDataId> serviceName resultDataTblName (fun e -> e |> Guid.Parse |> ResultDataId)
    let loadResultDataAllFs serviceName () = loadObjects<ResultDataWithId, Guid> serviceName resultDataTblName Guid.Parse

    let saveChartInfoFs serviceName (c : ChartInfo) = saveData<ChartInfo, Guid> serviceName chartInfoTblName c.resultDataId.value c
    let loadChartInfoFs serviceName (ResultDataId resultDataId) = loadData<ChartInfo, Guid> serviceName chartInfoTblName resultDataId
    let tryDeleteChartInfoFs serviceName (ResultDataId resultDataId) = tryDeleteData<ChartInfo, Guid> serviceName chartInfoTblName resultDataId
    let getChartInfoIdsFs serviceName () = getObjectIds<ResultDataId> serviceName chartInfoTblName (fun e -> e |> Guid.Parse |> ResultDataId)
    let loadChartInfoAllFs serviceName () = loadObjects<ChartInfo, Guid> serviceName chartInfoTblName Guid.Parse


    let saveLocalChartInfo d (c : ChartInfo) =
        let w() =
            try
                let getFileName (name : string) =
                    match d with
                    | Some (f, g) -> Path.Combine(f, g.ToString(), Path.GetFileName name)
                    | None -> name

                let saveChart (f : string) c =
                    let folder = Path.GetDirectoryName f
                    Directory.CreateDirectory(folder) |> ignore
                    File.WriteAllText(f, c)

                c.charts
                |> List.map (fun e -> saveChart (getFileName e.fileName) e.htmlContent)
                |> ignore
                Ok ()
            with
            | e -> e |> SaveChartsExn |> FileErr |> Error

        tryRopFun (fun e -> e |> GeneralFileExn |> FileErr) w


    let saveWorkerNodeInfoFs serviceName (r : WorkerNodeInfo) = saveData<WorkerNodeInfo, Guid> serviceName workerNodeInfoTblName r.workerNodeId.value.value r
    let loadWorkerNodeInfoFs serviceName (WorkerNodeId (MessagingClientId workerNodeId)) = loadData<WorkerNodeInfo, Guid> serviceName workerNodeInfoTblName workerNodeId
    let tryDeleteWorkerNodeInfoFs serviceName (WorkerNodeId (MessagingClientId workerNodeId)) = tryDeleteData<WorkerNodeInfo, Guid> serviceName workerNodeInfoTblName workerNodeId
    let getWorkerNodeInfoIdsFs serviceName () = getObjectIds<WorkerNodeId> serviceName workerNodeInfoTblName (fun e -> e |> Guid.Parse |> MessagingClientId |> WorkerNodeId)
    let loadeWorkerNodeInfoAllFs serviceName () = loadObjects<WorkerNodeInfo, Guid> serviceName workerNodeInfoTblName Guid.Parse

    let saveSolverRunnerErrFs serviceName (r : SolverRunnerCriticalError) = saveErrData<SolverRunnerCriticalError, Guid> serviceName solverRunnerErrTblName r.errorId.value r
