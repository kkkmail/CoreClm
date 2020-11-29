namespace ClmImpure

open Clm.ReactionRates
open Clm.CalculationData
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

module ReactionRateModelsExt =

    type ReactionRateModel
        with

        static member createAll (p : list<ReactionRateModelParamWithUsage>) (si : SubstInfo) =
            (
                [
                    FoodCreationModel.tryCreate
                    WasteRemovalModel.tryCreate
                    WasteRecyclingModel.tryCreate
                    SynthesisModel.tryCreate
                    SugarSynthesisModel.tryCreate
                    DestructionModel.tryCreate
                    CatalyticSynthesisModel.tryCreate si
                    EnCatalyticSynthesisModel.tryCreate si
                    CatalyticDestructionModel.tryCreate si
                    EnCatalyticDestructionModel.tryCreate si
                    LigationModel.tryCreate
                    CatalyticLigationModel.tryCreate si
                    EnCatalyticLigationModel.tryCreate si
                    SedimentationDirectModel.tryCreate si
                    SedimentationAllModel.tryCreate
                    RacemizationModel.tryCreate
                    CatalyticRacemizationModel.tryCreate si
                    EnCatalyticRacemizationModel.tryCreate si
                ]
                |> List.fold (fun acc r -> r acc) (p, [])
            )
            |> snd
