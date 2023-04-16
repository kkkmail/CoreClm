namespace ContGen

open System
open ClmSys
open ClmSys.SolverRunnerPrimitives
open Primitives.GeneralPrimitives
open Softellect.Sys.Rop

open Clm.ModelParams
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.ModelGeneratorErrors
open ServiceProxy.ModelGeneratorProxy
open Clm.Generator.ClmModelData
open Clm.Generator.ClmModel
open DbData.DatabaseTypes
open ClmSys.Logging
open ClmSys.TimerEvents

module ModelGenerator =

    let private toError g f = f |> g |> ModelGeneratorErr |> Error
    let private addError g f e = ((f |> g |> ModelGeneratorErr) + e) |> Error


    let generateModelCode (model : ClmModel) (c : ClmTask) fno =
        let addError = addError GenerateModelCodeErr

        match model.generateCode fno with
        | Ok r -> Ok r
        | Error e -> addError (UnableSaveModelCodeErr c.clmTaskInfo.clmTaskId) e


    let generateModel (proxy : GenerateModelProxy) (c : ClmTask) =
        let addError = addError GenerateModelErr
        let toError = toError GenerateModelErr
        let modelDataId = ModelDataId.getNewId()

        if c.remainingRepetitions > 0
        then
            match proxy.loadParams c with
            | Ok a ->
                let model = ClmModel (a.modelGenerationParams, modelDataId, c.clmTaskInfo.clmTaskId)

                match model.getModelData() |> proxy.upsertModelData with
                | Ok() ->
                    let r =
                        a.modelCommandLineParams
                        |> List.map(fun e ->
                                        {
                                            runQueueId = RunQueueId.getNewId()
                                            info =
                                                {
                                                    modelDataId = modelDataId;
                                                    defaultValueId = c.clmTaskInfo.taskDetails.clmDefaultValueId;
                                                    modelCommandLineParam = e
                                                }
                                            runQueueStatus = NotStartedRunQueue
                                            workerNodeIdOpt = None
                                            progressData = ClmProgressData.defaultValue
                                            createdOn = DateTime.Now
                                        })
                        |> List.map proxy.upsertRunQueue
                        |> foldUnitResults

                    let r1 = proxy.updateClmTask { c with remainingRepetitions = max (c.remainingRepetitions - 1) 0 }
                    combineUnitResults r r1 |> mapSuccessValue model
                | Error e -> addError (UnableUpsertModelDataErr c.clmTaskInfo.clmTaskId) e
            | Error e -> addError (GenerateModelError.UnableLoadParamsErr c.clmTaskInfo.clmTaskId) e
        else toError (TaskCompletedErr c.clmTaskInfo.clmTaskId)


    let generateAll (proxy : GenerateAllProxy) =
        let rec inner() =
            let r, f = proxy.loadIncompleteClmTasks() |> unzipListResult
            let e = r |> List.map proxy.generateModel |> foldUnitResults
            let f1 = f |> foldToUnitResult |> combineUnitResults e

            match r, f1 with
            | [], Ok() -> Ok()
            | _ :: _, Ok() -> inner()
            | _, Error e -> Error e

        inner()


    type GenerateAllProxy
        with

        static member create u coll so c =
            {
                loadIncompleteClmTasks = fun () -> loadIncompleteClmTasks c
                generateModel = (generateModel (GenerateModelProxy.create u coll so c)) >> (mapSuccessValue ())
            }


    let createModelGenerator (logger : Logger) u coll so c =
        logger.logInfoString "createModelGenerator: Creating model generator..."
        let proxy = GenerateAllProxy.create u coll so c
        let e = fun () -> generateAll proxy
        let h = ClmEventHandler(ClmEventHandlerInfo.defaultValue logger e "ModelGenerator - generateAll")
        h
