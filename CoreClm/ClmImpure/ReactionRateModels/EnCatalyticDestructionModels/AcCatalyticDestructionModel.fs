namespace ClmImpure.ReactionRateModels

open ClmImpure.ReactionRateModels.AcCatalyticDestructionRandomModel
open ClmImpure.ReactionRateModels.AcCatalyticDestructionSimilarModel

module AcCatalyticDestructionModel =

    type AcCatalyticDestructionParamWithModel =
        | AcCatDestrRndParamWithModel of AcCatalyticDestructionRandomParamWithModel
        | AcCatDestrSimParamWithModel of AcCatalyticDestructionSimilarParamWithModel


    type AcCatalyticDestructionModel =
        | AcCatDestrRndModel of AcCatalyticDestructionRandomModel
        | AcCatDestrSimModel of AcCatalyticDestructionSimilarModel

        member model.getRates rnd t r =
            match model with
            | AcCatDestrRndModel m -> m.getRates rnd t r
            | AcCatDestrSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | AcCatDestrRndModel m -> m.inputParams |> AcCatDestrRndParamWithModel
            | AcCatDestrSimModel m -> m.inputParams |> AcCatDestrSimParamWithModel

        member model.getAllRates() =
            match model with
            | AcCatDestrRndModel m -> m.getAllRates()
            | AcCatDestrSimModel m -> m.getAllRates()

        static member create p =
            match p with
            | AcCatDestrRndParamWithModel q -> AcCatalyticDestructionRandomModel q |> AcCatDestrRndModel
            | AcCatDestrSimParamWithModel q -> AcCatalyticDestructionSimilarModel q |> AcCatDestrSimModel
