namespace Clm.Generator

open ClmImpure.RateProvider
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.FoodCreationModel
open ClmImpure.ReactionRateModels.WasteRecyclingModel
open ClmImpure.ReactionRateModels.WasteRemovalModel
open ClmImpure.ReactionRateModels.SynthesisModel
open ClmImpure.ReactionRateModels.SugarSynthesisModel
open ClmImpure.ReactionRateModels.DestructionModel
open ClmImpure.ReactionRateModels.LigationModel
open ClmImpure.ReactionRateModels.RacemizationModel
open ClmImpure.ReactionRateModels.CatalyticDestructionModel
open ClmImpure.ReactionRateModels.EnCatalyticDestructionModel
open ClmImpure.ReactionRateModels.AcCatalyticDestructionModel
open ClmImpure.ReactionRateModels.CatalyticLigationModel
open ClmImpure.ReactionRateModels.EnCatalyticLigationModel
open ClmImpure.ReactionRateModels.AcFwdCatalyticLigationModel
open ClmImpure.ReactionRateModels.AcBkwCatalyticLigationModel
open ClmImpure.ReactionRateModels.CatalyticRacemizationModel
open ClmImpure.ReactionRateModels.EnCatalyticRacemizationModel
open ClmImpure.ReactionRateModels.AcCatalyticRacemizationModel
open ClmImpure.ReactionRateModels.CatalyticSynthesisModel
open ClmImpure.ReactionRateModels.EnCatalyticSynthesisModel
open ClmImpure.ReactionRateModels.AcCatalyticSynthesisModel
open ClmImpure.ReactionRateModels.SedimentationDirectModel
open ClmImpure.ReactionRateModels.SedimentationAllModel
open ClmImpure.ReactionRateModels.ActivationModel

module ReactionRatesExt =

    type FoodCreationModel
        with
        member model.primaryDistribution = None


    type WasteRemovalModel
        with
        member model.primaryDistribution = None


    type WasteRecyclingModel
        with
        member model.primaryDistribution = None


    type SynthesisModel
        with
        member model.primaryDistribution =
            match model with
            | SynthRndModel m -> Some m.inputParams.synthesisDistribution


    type SugarSynthesisModel
        with
        member model.primaryDistribution =
            match model with
            | SugarSynthRndModel m -> Some m.inputParams.sugarSynthesisDistribution


    type DestructionModel
        with
        member model.primaryDistribution =
            match model with
            | DestrRndModel m -> Some m.inputParams.destructionDistribution


    type CatalyticSynthesisModel
        with
        member model.primaryDistribution =
            match model with
            | CatSynthRndModel m ->
                m.inputParams.catSynthRndParam.catSynthRndEeParams.rateMultiplierDistr.value
            | CatSynthSimModel m ->
                m.inputParams.catSynthModel.inputParams.catSynthRndParam.catSynthRndEeParams.rateMultiplierDistr.value


    type EnCatalyticSynthesisModel
        with
        member model.primaryDistribution =
            match model with
            | EnCatSynthRndModel m ->
                m.inputParams.enCatSynthRndParam.enCatSynthRndEeParams.rateMultiplierDistr.value
            | EnCatSynthSimModel m ->
                m.inputParams.enCatSynthModel.inputParams.enCatSynthRndParam.enCatSynthRndEeParams.rateMultiplierDistr.value


    type AcCatalyticSynthesisModel
        with
        member model.primaryDistribution =
            match model with
            | AcCatSynthRndModel m ->
                m.inputParams.acCatSynthRndParam.acCatSynthRndEeParams.rateMultiplierDistr.value
            | AcCatSynthSimModel m ->
                m.inputParams.acCatSynthModel.inputParams.acCatSynthRndParam.acCatSynthRndEeParams.rateMultiplierDistr.value


    type CatalyticDestructionModel
        with
        member model.primaryDistribution =
            match model with
            | CatDestrRndModel m ->
                m.inputParams.catDestrRndParam.catDestrRndEeParams.rateMultiplierDistr.value
            | CatDestrSimModel m ->
                m.inputParams.catDestrModel.inputParams.catDestrRndParam.catDestrRndEeParams.rateMultiplierDistr.value


    type EnCatalyticDestructionModel
        with
        member model.primaryDistribution =
            match model with
            | EnCatDestrRndModel m ->
                m.inputParams.enCatDestrRndParam.enCatDestrRndEeParams.rateMultiplierDistr.value
            | EnCatDestrSimModel m ->
                m.inputParams.enCatDestrModel.inputParams.enCatDestrRndParam.enCatDestrRndEeParams.rateMultiplierDistr.value


    type AcCatalyticDestructionModel
        with
        member model.primaryDistribution =
            match model with
            | AcCatDestrRndModel m ->
                m.inputParams.acCatDestrRndParam.acCatDestrRndEeParams.rateMultiplierDistr.value
            | AcCatDestrSimModel m ->
                m.inputParams.acCatDestrModel.inputParams.acCatDestrRndParam.acCatDestrRndEeParams.rateMultiplierDistr.value


    type LigationModel
        with
        member model.primaryDistribution =
            match model with
            | LigRndModel m -> Some m.inputParams.ligationDistribution


    type CatalyticLigationModel
        with
        member model.primaryDistribution =
            match model with
            | CatLigRndModel m ->
                m.inputParams.catLigationParam.catLigRndEeParams.rateMultiplierDistr.value
            | CatLigSimModel m ->
                m.inputParams.catLigModel.inputParams.catLigationParam.catLigRndEeParams.rateMultiplierDistr.value


    type EnCatalyticLigationModel
        with
        member model.primaryDistribution =
            match model with
            | EnCatLigRndModel m ->
                m.inputParams.enCatLigationParam.enCatLigRndEeParams.rateMultiplierDistr.value
            | EnCatLigSimModel m ->
                m.inputParams.enCatLigModel.inputParams.enCatLigationParam.enCatLigRndEeParams.rateMultiplierDistr.value


    type AcFwdCatalyticLigationModel
        with
        member model.primaryDistribution =
            match model with
            | AcFwdCatLigRndModel m ->
                m.inputParams.acFwdCatLigationParam.acFwdCatLigRndEeParams.rateMultiplierDistr.value
            | AcFwdCatLigSimModel m ->
                m.inputParams.acFwdCatLigModel.inputParams.acFwdCatLigationParam.acFwdCatLigRndEeParams.rateMultiplierDistr.value


    type AcBkwCatalyticLigationModel
        with
        member model.primaryDistribution =
            match model with
            | AcBkwCatLigRndModel m ->
                m.inputParams.acBkwCatLigationParam.acBkwCatLigRndEeParams.rateMultiplierDistr.value
            | AcBkwCatLigSimModel m ->
                m.inputParams.acBkwCatLigModel.inputParams.acBkwCatLigationParam.acBkwCatLigRndEeParams.rateMultiplierDistr.value


    type SedimentationDirectModel
        with
        member model.primaryDistribution =
            match model with
            | SedDirRndModel m -> Some m.inputParams.sedDirDistribution
            | SedDirSimModel m -> Some m.inputParams.sedDirModel.inputParams.sedDirDistribution


    type SedimentationAllModel
        with
        member model.primaryDistribution =
            match model with
            | SedAllRndModel m -> Some m.inputParams.sedimentationAllDistribution


    type RacemizationModel
        with
        member model.primaryDistribution =
            match model with
            | RacemRndModel m -> Some m.inputParams.racemizationDistribution


    type CatalyticRacemizationModel
        with
        member model.primaryDistribution =
            match model with
            | CatRacemRndModel m ->
                m.inputParams.catRacemRndParam.catRacemRndEeParams.rateMultiplierDistr.value
            | CatRacemSimModel m ->
                m.inputParams.catRacemModel.inputParams.catRacemRndParam.catRacemRndEeParams.rateMultiplierDistr.value


    type EnCatalyticRacemizationModel
        with
        member model.primaryDistribution =
            match model with
            | EnCatRacemRndModel m ->
                m.inputParams.enCatRacemRndParam.enCatRacemRndEeParams.rateMultiplierDistr.value
            | EnCatRacemSimModel m ->
                m.inputParams.enCatRacemModel.inputParams.enCatRacemRndParam.enCatRacemRndEeParams.rateMultiplierDistr.value


    type AcCatalyticRacemizationModel
        with
        member model.primaryDistribution =
            match model with
            | AcCatRacemRndModel m ->
                m.inputParams.acCatRacemRndParam.acCatRacemRndEeParams.rateMultiplierDistr.value
            | AcCatRacemSimModel m ->
                m.inputParams.acCatRacemModel.inputParams.acCatRacemRndParam.acCatRacemRndEeParams.rateMultiplierDistr.value


    type ActivationModel
        with
        member model.primaryDistribution = failwith ""


    type ReactionRateModel
        with
        member model.primaryDistribution =
            match model with
            | FoodCreationRateModel m -> m.primaryDistribution
            | WasteRemovalRateModel m -> m.primaryDistribution
            | WasteRecyclingRateModel m -> m.primaryDistribution
            | SynthesisRateModel m -> m.primaryDistribution
            | SugarSynthesisRateModel m -> m.primaryDistribution
            | DestructionRateModel m -> m.primaryDistribution
            | CatalyticSynthesisRateModel m -> m.primaryDistribution
            | EnCatalyticSynthesisRateModel m -> m.primaryDistribution
            | AcCatalyticSynthesisRateModel m -> m.primaryDistribution
            | CatalyticDestructionRateModel m -> m.primaryDistribution
            | EnCatalyticDestructionRateModel m -> m.primaryDistribution
            | AcCatalyticDestructionRateModel m -> m.primaryDistribution
            | LigationRateModel m -> m.primaryDistribution
            | CatalyticLigationRateModel m -> m.primaryDistribution
            | EnCatalyticLigationRateModel m -> m.primaryDistribution
            | AcFwdCatalyticLigationRateModel m -> m.primaryDistribution
            | AcBkwCatalyticLigationRateModel m -> m.primaryDistribution
            | SedimentationDirectRateModel m -> m.primaryDistribution
            | SedimentationAllRateModel m -> m.primaryDistribution
            | RacemizationRateModel m -> m.primaryDistribution
            | CatalyticRacemizationRateModel m -> m.primaryDistribution
            | EnCatalyticRacemizationRateModel m -> m.primaryDistribution
            | AcCatalyticRacemizationRateModel m -> m.primaryDistribution
            | ActivationRateModel m -> m.primaryDistribution


    type ReactionRateProvider
        with
        member this.tryGetPrimaryDistribution a = this.tryGetModel a |> Option.bind (fun m -> m.primaryDistribution)
