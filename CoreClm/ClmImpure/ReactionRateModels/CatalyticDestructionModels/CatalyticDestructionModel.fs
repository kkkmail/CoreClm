namespace ClmImpure.ReactionRateModels

open ClmImpure.ReactionRateModels.CatalyticDestructionRandomModel
open ClmImpure.ReactionRateModels.CatalyticDestructionSimilarModel

module CatalyticDestructionModel =

    type CatalyticDestructionParamWithModel =
        | CatDestrRndParamWithModel of CatalyticDestructionRandomParamWithModel
        | CatDestrSimParamWithModel of CatalyticDestructionSimilarParamWithModel


    type CatalyticDestructionModel =
        | CatDestrRndModel of CatalyticDestructionRandomModel
        | CatDestrSimModel of CatalyticDestructionSimilarModel

        member model.getRates rnd t r =
            match model with
            | CatDestrRndModel m -> m.getRates rnd t r
            | CatDestrSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | CatDestrRndModel m -> m.inputParams |> CatDestrRndParamWithModel
            | CatDestrSimModel m -> m.inputParams |> CatDestrSimParamWithModel

        member model.getAllRates() =
            match model with
            | CatDestrRndModel m -> m.getAllRates()
            | CatDestrSimModel m -> m.getAllRates()

        static member create p =
            match p with
            | CatDestrRndParamWithModel q -> CatalyticDestructionRandomModel q |> CatDestrRndModel
            | CatDestrSimParamWithModel q -> CatalyticDestructionSimilarModel q |> CatDestrSimModel
