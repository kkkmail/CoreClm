namespace ContGen

open System
open ClmSys
open ClmSys.SolverRunnerPrimitives
//open Primitives.GeneralPrimitives
open Softellect.Sys.Rop
open Softellect.Sys.TimerEvents

open Clm.ModelParams
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
//open ClmSys.GeneralPrimitives
open ClmSys.ModelGeneratorErrors
open ServiceProxy.ModelGeneratorProxy
open Clm.Generator.ClmModelData
open Clm.Generator.ClmModel
open DbData.DatabaseTypesDbo
open DbData.DatabaseTypesClm
//open ClmSys.Logging
//open ClmSys.TimerEvents
open Clm.ClmData
open Clm.ModelInit

open Softellect.DistributedProcessing.ModelGenerator.Program
open Softellect.DistributedProcessing.Proxy.ModelGenerator
open Softellect.Sys.ExitErrorCodes
open Softellect.DistributedProcessing.Primitives.Common
open Softellect.Sys.Logging
open Clm.Distributions

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

                let modelData = model.getModelData()

                let g p =
                    let i =
                        {
                            defaultValueId = c.clmTaskInfo.taskDetails.clmDefaultValueId
                            modelCommandLineParam = p
                            modelData = modelData
                        }

                    let inputParams =
                        {
                            startTime = EvolutionTime.defaultValue
                            endTime = p.tEnd |> decimal |> EvolutionTime
                        }

                    let outputParams = SolverOutputParams.defaultValue
                    let modelDataParamsWithExtraData = modelData.modelData.getModelDataParamsWithExtraData()
                    let rnd = RandomValueGetter.create modelData.seedValue

                    let generateModelContext i =
                        {
                            derivativeCalculator = modelData.modelData.modelBinaryData.calculationData.derivativeCalculator
                            evolutionTime = inputParams.endTime
                            initialValues = defaultInit rnd (ModelInitValuesParams.getDefaultValue modelDataParamsWithExtraData p.useAbundant) (double p.y0)
                        }

                    let proxy :  UserProxy<ClmInitialData, ClmSolverContext> =
                        {
                            getInitialData = fun () -> i
                            generateModelContext = generateModelContext
                            getSolverInputParams = fun _ -> inputParams
                            getSolverOutputParams = fun _ -> outputParams
                        }

                    let systemProxy = SystemProxy.create()
                    let result = generateModel<ClmInitialData, ClmSolverContext> systemProxy clmSolverId proxy
                    printfn $"result: '%A{result}'."

                let r =
                    a.modelCommandLineParams
                    |> List.map g


                failwith ""

                //match model.getModelData() |> proxy.upsertModelData with
                //| Ok() ->
                //    let r =
                //        a.modelCommandLineParams
                //        |> List.map(fun e ->
                //                        {
                //                            runQueueId = RunQueueId.getNewId()
                //                            info =
                //                                {
                //                                    modelDataId = modelDataId;
                //                                    defaultValueId = c.clmTaskInfo.taskDetails.clmDefaultValueId;
                //                                    modelCommandLineParam = e
                //                                }
                //                            runQueueStatus = NotStartedRunQueue
                //                            workerNodeIdOpt = None
                //                            progressData = ClmProgressData.defaultValue
                //                            createdOn = DateTime.Now
                //                        })
                //        |> List.map proxy.upsertRunQueue
                //        |> foldUnitResults

                //    let r1 = proxy.updateClmTask { c with remainingRepetitions = max (c.remainingRepetitions - 1) 0 }
                //    combineUnitResults r r1 |> mapSuccessValue model
                //| Error e -> addError (UnableUpsertModelDataErr c.clmTaskInfo.clmTaskId) e
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
        logger.logInfo "createModelGenerator: Creating model generator..."
        let proxy = GenerateAllProxy.create u coll so c
        let generateAll() = generateAll proxy
        let toError e = ClmTimerEventErr e
        let i = TimerEventHandlerInfo<ClmError>.defaultValue toError generateAll "ModelGenerator - generateAll"
        let h = TimerEventHandler i
        h.start()
        h
