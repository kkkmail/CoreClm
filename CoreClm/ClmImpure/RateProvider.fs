namespace ClmImpure

open FSharp.Collections

open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRates
open Clm.CalculationData

open ClmImpure.ReactionRateModels
open ClmImpure.ReactionRateModelsExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.FoodCreationModel
open ClmImpure.ReactionRateModelExtensions.FoodCreationModelExt
open ClmImpure.ReactionRateModels.WasteRecyclingModel
open ClmImpure.ReactionRateModelExtensions.WasteRecyclingModelExt
open ClmImpure.ReactionRateModels.WasteRemovalModel
open ClmImpure.ReactionRateModelExtensions.WasteRemovalModelExt
open ClmImpure.ReactionRateModels.SynthesisModel
open ClmImpure.ReactionRateModelExtensions.SynthesisModelExt
open ClmImpure.ReactionRateModels.SugarSynthesisModel
open ClmImpure.ReactionRateModelExtensions.SugarSynthesisModelExt
open ClmImpure.ReactionRateModels.DestructionModel
open ClmImpure.ReactionRateModelExtensions.DestructionModelExt
open ClmImpure.ReactionRateModels.LigationModel
open ClmImpure.ReactionRateModelExtensions.LigationModelExt
open ClmImpure.ReactionRateModels.RacemizationModel
open ClmImpure.ReactionRateModelExtensions.RacemizationModelExt
open ClmImpure.ReactionRateModels.CatalyticDestructionModel
open ClmImpure.ReactionRateModelExtensions.CatalyticDestructionModelExt
open ClmImpure.ReactionRateModels.EnCatalyticDestructionModel
open ClmImpure.ReactionRateModelExtensions.EnCatalyticDestructionModelExt
open ClmImpure.ReactionRateModels.CatalyticLigationModel
open ClmImpure.ReactionRateModelExtensions.CatalyticLigationModelExt
open ClmImpure.ReactionRateModels.EnCatalyticLigationModel
open ClmImpure.ReactionRateModelExtensions.EnCatalyticLigationModelExt
open ClmImpure.ReactionRateModels.CatalyticRacemizationModel
open ClmImpure.ReactionRateModelExtensions.CatalyticRacemizationModelExt
open ClmImpure.ReactionRateModels.EnCatalyticRacemizationModel
open ClmImpure.ReactionRateModelExtensions.EnCatalyticRacemizationModelExt
open ClmImpure.ReactionRateModels.CatalyticSynthesisModel
open ClmImpure.ReactionRateModelExtensions.CatalyticSynthesisModelExt
open ClmImpure.ReactionRateModels.EnCatalyticSynthesisModel
open ClmImpure.ReactionRateModelExtensions.EnCatalyticSynthesisModelExt
open ClmImpure.ReactionRateModels.SedimentationDirectModel
open ClmImpure.ReactionRateModelExtensions.SedimentationDirectModelExt
open ClmImpure.ReactionRateModels.SedimentationAllModel
open ClmImpure.ReactionRateModelExtensions.SedimentationAllModelExt

module RateProvider =

    type ReactionRateProvider (p: ReactionRateProviderParams, si : SubstInfo) =
        let allModels =
            let x = ReactionRateModel.createAll (p.allParams()) si
            x

        let tryPick getter = allModels |> List.tryPick getter

        let getRatesImpl rnd t a =
            match a with
            | FoodCreation r -> tryPick FoodCreationModel.modelGetter |> bind (fun m -> m.getRates r)
            | WasteRemoval r -> tryPick WasteRemovalModel.modelGetter |> bind (fun m -> m.getRates r)
            | WasteRecycling r -> tryPick WasteRecyclingModel.modelGetter |> bind (fun m -> m.getRates r)
            | Synthesis r -> tryPick SynthesisModel.modelGetter |> bind (fun m -> m.getRates rnd r)
            | SugarSynthesis r -> tryPick SugarSynthesisModel.modelGetter |> bind (fun m -> m.getRates rnd r)
            | Destruction r -> tryPick DestructionModel.modelGetter |> bind (fun m -> m.getRates rnd r)
            | CatalyticSynthesis r -> tryPick CatalyticSynthesisModel.modelGetter |> bind (fun m -> m.getRates rnd t r)
            | EnCatalyticSynthesis r -> tryPick EnCatalyticSynthesisModel.modelGetter |> bind (fun m -> m.getRates rnd t r)
            | CatalyticDestruction r -> tryPick CatalyticDestructionModel.modelGetter |> bind (fun m -> m.getRates rnd t r)
            | EnCatalyticDestruction r -> tryPick EnCatalyticDestructionModel.modelGetter |> bind (fun m -> m.getRates rnd t r)
            | Ligation r -> tryPick LigationModel.modelGetter |> bind (fun m -> m.getRates rnd r)
            | CatalyticLigation r -> tryPick CatalyticLigationModel.modelGetter |> bind (fun m -> m.getRates rnd t r)
            | EnCatalyticLigation r -> tryPick EnCatalyticLigationModel.modelGetter |> bind (fun m -> m.getRates rnd t r)
            | SedimentationDirect r -> tryPick SedimentationDirectModel.modelGetter |> bind (fun m -> m.getRates rnd t r)
            | SedimentationAll r -> tryPick SedimentationAllModel.modelGetter |> bind (fun m -> m.getRates rnd r)
            | Racemization r -> tryPick RacemizationModel.modelGetter|> bind (fun m -> m.getRates rnd r)
            | CatalyticRacemization r -> tryPick CatalyticRacemizationModel.modelGetter |> bind (fun m -> m.getRates rnd t r)
            | EnCatalyticRacemization r -> tryPick EnCatalyticRacemizationModel.modelGetter |> bind (fun m -> m.getRates rnd t r)

        let tryGetModelImpl n =
            match n with
            | FoodCreationName -> tryPick FoodCreationModel.modelGetter |> Option.bind(fun e -> FoodCreationRateModel e |> Some)
            | WasteRemovalName -> tryPick WasteRemovalModel.modelGetter |> Option.bind(fun e -> WasteRemovalRateModel e |> Some)
            | WasteRecyclingName -> tryPick WasteRecyclingModel.modelGetter |> Option.bind(fun e -> WasteRecyclingRateModel e |> Some)
            | SynthesisName -> tryPick SynthesisModel.modelGetter |> Option.bind(fun e -> SynthesisRateModel e |> Some)
            | SugarSynthesisName -> tryPick SugarSynthesisModel.modelGetter |> Option.bind(fun e -> SugarSynthesisRateModel e |> Some)
            | DestructionName -> tryPick DestructionModel.modelGetter |> Option.bind(fun e -> DestructionRateModel e |> Some)
            | CatalyticSynthesisName -> tryPick CatalyticSynthesisModel.modelGetter |> Option.bind(fun e -> CatalyticSynthesisRateModel e |> Some)
            | EnCatalyticSynthesisName -> tryPick EnCatalyticSynthesisModel.modelGetter |> Option.bind(fun e -> EnCatalyticSynthesisRateModel e |> Some)
            | CatalyticDestructionName -> tryPick CatalyticDestructionModel.modelGetter |> Option.bind(fun e -> CatalyticDestructionRateModel e |> Some)
            | EnCatalyticDestructionName -> tryPick EnCatalyticDestructionModel.modelGetter |> Option.bind(fun e -> EnCatalyticDestructionRateModel e |> Some)
            | LigationName -> tryPick LigationModel.modelGetter |> Option.bind(fun e -> LigationRateModel e |> Some)
            | CatalyticLigationName -> tryPick CatalyticLigationModel.modelGetter |> Option.bind(fun e -> CatalyticLigationRateModel e |> Some)
            | EnCatalyticLigationName -> tryPick EnCatalyticLigationModel.modelGetter |> Option.bind(fun e -> EnCatalyticLigationRateModel e |> Some)
            | SedimentationDirectName -> tryPick SedimentationDirectModel.modelGetter |> Option.bind(fun e -> SedimentationDirectRateModel e |> Some)
            | SedimentationAllName -> tryPick SedimentationAllModel.modelGetter |> Option.bind(fun e -> SedimentationAllRateModel e |> Some)
            | RacemizationName -> tryPick RacemizationModel.modelGetter |> Option.bind(fun e -> RacemizationRateModel e |> Some)
            | CatalyticRacemizationName -> tryPick CatalyticRacemizationModel.modelGetter |> Option.bind(fun e -> CatalyticRacemizationRateModel e |> Some)
            | EnCatalyticRacemizationName -> tryPick EnCatalyticRacemizationModel.modelGetter |> Option.bind(fun e -> EnCatalyticRacemizationRateModel e |> Some)

        member __.providerParams = p
        member __.getRates rnd a = getRatesImpl rnd a
        member __.tryGetModel n = tryGetModelImpl n
        member __.getAllRates() = allModels |> List.map (fun m -> m.model.getAllRates())
