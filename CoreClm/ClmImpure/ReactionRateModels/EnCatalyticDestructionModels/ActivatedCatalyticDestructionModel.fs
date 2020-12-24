namespace ClmImpure.ReactionRateModels

open ClmImpure.ReactionRateModels.ActivatedCatalyticDestructionRandomModel
open ClmImpure.ReactionRateModels.ActivatedCatalyticDestructionSimilarModel

module ActivatedCatalyticDestructionModel =

    type ActivatedCatalyticDestructionParamWithModel =
        | ActivatedCatDestrRndParamWithModel of ActivatedCatalyticDestructionRandomParamWithModel
        | ActivatedCatDestrSimParamWithModel of ActivatedCatalyticDestructionSimilarParamWithModel


    type ActivatedCatalyticDestructionModel =
        | ActivatedCatDestrRndModel of ActivatedCatalyticDestructionRandomModel
        | ActivatedCatDestrSimModel of ActivatedCatalyticDestructionSimilarModel

        member model.getRates rnd t r =
            match model with
            | ActivatedCatDestrRndModel m -> m.getRates rnd t r
            | ActivatedCatDestrSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | ActivatedCatDestrRndModel m -> m.inputParams |> ActivatedCatDestrRndParamWithModel
            | ActivatedCatDestrSimModel m -> m.inputParams |> ActivatedCatDestrSimParamWithModel

        member model.getAllRates() =
            match model with
            | ActivatedCatDestrRndModel m -> m.getAllRates()
            | ActivatedCatDestrSimModel m -> m.getAllRates()

        static member create p =
            match p with
            | ActivatedCatDestrRndParamWithModel q -> ActivatedCatalyticDestructionRandomModel q |> ActivatedCatDestrRndModel
            | ActivatedCatDestrSimParamWithModel q -> ActivatedCatalyticDestructionSimilarModel q |> ActivatedCatDestrSimModel
