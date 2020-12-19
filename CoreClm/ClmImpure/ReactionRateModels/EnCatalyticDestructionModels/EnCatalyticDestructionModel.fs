namespace ClmImpure.ReactionRateModels

open ClmImpure.ReactionRateModels.EnCatalyticDestructionRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticDestructionSimilarModel

module EnCatalyticDestructionModel =

    type EnCatalyticDestructionParamWithModel =
        | EnCatDestrRndParamWithModel of EnCatalyticDestructionRandomParamWithModel
        | EnCatDestrSimParamWithModel of EnCatalyticDestructionSimilarParamWithModel


    type EnCatalyticDestructionModel =
        | EnCatDestrRndModel of EnCatalyticDestructionRandomModel
        | EnCatDestrSimModel of EnCatalyticDestructionSimilarModel

        member model.getRates rnd t r =
            match model with
            | EnCatDestrRndModel m -> m.getRates rnd t r
            | EnCatDestrSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | EnCatDestrRndModel m -> m.inputParams |> EnCatDestrRndParamWithModel
            | EnCatDestrSimModel m -> m.inputParams |> EnCatDestrSimParamWithModel

        member model.getAllRates() =
            match model with
            | EnCatDestrRndModel m -> m.getAllRates()
            | EnCatDestrSimModel m -> m.getAllRates()

        static member create p =
            match p with
            | EnCatDestrRndParamWithModel q -> EnCatalyticDestructionRandomModel q |> EnCatDestrRndModel
            | EnCatDestrSimParamWithModel q -> EnCatalyticDestructionSimilarModel q |> EnCatDestrSimModel
