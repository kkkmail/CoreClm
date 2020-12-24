namespace ClmImpure

open Clm.ReactionRates
open ClmImpure.ReactionRateModels.FoodCreationModel
open ClmImpure.ReactionRateModels.WasteRecyclingModel
open ClmImpure.ReactionRateModels.WasteRemovalModel
open ClmImpure.ReactionRateModels.SugarSynthesisModel
open ClmImpure.ReactionRateModels.DestructionModel
open ClmImpure.ReactionRateModels.LigationModel
open ClmImpure.ReactionRateModels.RacemizationModel
open ClmImpure.ReactionRateModels.SynthesisModel
open ClmImpure.ReactionRateModels.CatalyticDestructionModel
open ClmImpure.ReactionRateModels.CatalyticLigationModel
open ClmImpure.ReactionRateModels.CatalyticRacemizationModel
open ClmImpure.ReactionRateModels.CatalyticSynthesisModel
open ClmImpure.ReactionRateModels.SedimentationDirectModel
open ClmImpure.ReactionRateModels.SedimentationAllModel
open ClmImpure.ReactionRateModels.EnCatalyticSynthesisModel
open ClmImpure.ReactionRateModels.EnCatalyticLigationModel
open ClmImpure.ReactionRateModels.EnCatalyticDestructionModel
open ClmImpure.ReactionRateModels.EnCatalyticRacemizationModel
open ClmImpure.ReactionRateModels.AcCatalyticSynthesisModel
open ClmImpure.ReactionRateModels.AcFwdCatalyticLigationModel
open ClmImpure.ReactionRateModels.AcBkwCatalyticLigationModel
open ClmImpure.ReactionRateModels.AcCatalyticDestructionModel
open ClmImpure.ReactionRateModels.AcCatalyticRacemizationModel

module ReactionRateModelsAll =

    type ReactionRateModel =
        | FoodCreationRateModel of FoodCreationModel
        | WasteRemovalRateModel of WasteRemovalModel
        | WasteRecyclingRateModel of WasteRecyclingModel
        | SynthesisRateModel of SynthesisModel
        | SugarSynthesisRateModel of SugarSynthesisModel
        | DestructionRateModel of DestructionModel
        | CatalyticSynthesisRateModel of CatalyticSynthesisModel
        | EnCatalyticSynthesisRateModel of EnCatalyticSynthesisModel
        | AcCatalyticSynthesisRateModel of AcCatalyticSynthesisModel
        | CatalyticDestructionRateModel of CatalyticDestructionModel
        | EnCatalyticDestructionRateModel of EnCatalyticDestructionModel
        | AcCatalyticDestructionRateModel of AcCatalyticDestructionModel
        | LigationRateModel of LigationModel
        | CatalyticLigationRateModel of CatalyticLigationModel
        | EnCatalyticLigationRateModel of EnCatalyticLigationModel
        | AcFwdCatalyticLigationRateModel of AcFwdCatalyticLigationModel
        | AcBkwCatalyticLigationRateModel of AcBkwCatalyticLigationModel
        | SedimentationDirectRateModel of SedimentationDirectModel
        | SedimentationAllRateModel of SedimentationAllModel
        | RacemizationRateModel of RacemizationModel
        | CatalyticRacemizationRateModel of CatalyticRacemizationModel
        | EnCatalyticRacemizationRateModel of EnCatalyticRacemizationModel
        | AcCatalyticRacemizationRateModel of AcCatalyticRacemizationModel
        | ActivationRateModel of ActivationModel


        member rm.getAllRates() =
            match rm with
            | FoodCreationRateModel m -> m.getAllRates() |> FoodCreationRates
            | WasteRemovalRateModel m -> m.getAllRates() |> WasteRemovalRates
            | WasteRecyclingRateModel m -> m.getAllRates() |> WasteRecyclingRates
            | SynthesisRateModel m -> m.getAllRates() |> SynthesisRates
            | SugarSynthesisRateModel m -> m.getAllRates() |> SugarSynthesisRates
            | DestructionRateModel m -> m.getAllRates() |> DestructionRates
            | CatalyticSynthesisRateModel m -> m.getAllRates() |> CatalyticSynthesisRates
            | EnCatalyticSynthesisRateModel m -> m.getAllRates() |> EnCatalyticSynthesisRates
            | AcCatalyticSynthesisRateModel m -> m.getAllRates() |> AcCatalyticSynthesisRates
            | CatalyticDestructionRateModel m -> m.getAllRates() |> CatalyticDestructionRates
            | EnCatalyticDestructionRateModel m -> m.getAllRates() |> EnCatalyticDestructionRates
            | AcCatalyticDestructionRateModel m -> m.getAllRates() |> AcCatalyticDestructionRates
            | LigationRateModel m -> m.getAllRates() |> LigationRates
            | CatalyticLigationRateModel m -> m.getAllRates() |> CatalyticLigationRates
            | EnCatalyticLigationRateModel m -> m.getAllRates() |> EnCatalyticLigationRates
            | AcFwdCatalyticLigationRateModel m -> m.getAllRates() |> AcFwdCatalyticLigationRates
            | AcBkwCatalyticLigationRateModel m -> m.getAllRates() |> AcBkwCatalyticLigationRates
            | SedimentationDirectRateModel m -> m.getAllRates() |> SedimentationDirectRates
            | SedimentationAllRateModel m -> m.getAllRates() |> SedimentationAllRates
            | RacemizationRateModel m -> m.getAllRates() |> RacemizationRates
            | CatalyticRacemizationRateModel m -> m.getAllRates() |> CatalyticRacemizationRates
            | EnCatalyticRacemizationRateModel m -> m.getAllRates() |> EnCatalyticRacemizationRates
            | AcCatalyticRacemizationRateModel m -> m.getAllRates() |> AcCatalyticRacemizationRates
            | ActivationRateModel m -> m.getAllRates() |> ActivationRates


    type ReactionRateModelWithUsage =
        {
            model : ReactionRateModel
            usage : ReactionRateModelParamUsage
        }
