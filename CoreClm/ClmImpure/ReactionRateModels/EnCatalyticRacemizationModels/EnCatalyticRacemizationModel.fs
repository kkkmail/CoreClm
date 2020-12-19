namespace ClmImpure.ReactionRateModels

open ClmImpure.ReactionRateModels.EnCatalyticRacemizationRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticRacemizationSimilarModel

module EnCatalyticRacemizationModel =

    type EnCatalyticRacemizationParamWithModel =
        | EnCatRacemRndParamWithModel of EnCatalyticRacemizationRandomParamWithModel
        | EnCatRacemSimParamWithModel of EnCatalyticRacemizationSimilarParamWithModel


    type EnCatalyticRacemizationModel =
        | EnCatRacemRndModel of EnCatalyticRacemizationRandomModel
        | EnCatRacemSimModel of EnCatalyticRacemizationSimilarModel

        member model.getRates rnd t r =
            match model with
            | EnCatRacemRndModel m -> m.getRates rnd t r
            | EnCatRacemSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | EnCatRacemRndModel m -> m.inputParams |> EnCatRacemRndParamWithModel
            | EnCatRacemSimModel m -> m.inputParams |> EnCatRacemSimParamWithModel

        member model.getAllRates() =
            match model with
            | EnCatRacemRndModel m -> m.getAllRates()
            | EnCatRacemSimModel m -> m.getAllRates()

        static member create p =
            match p with
            | EnCatRacemRndParamWithModel q -> EnCatalyticRacemizationRandomModel q |> EnCatRacemRndModel
            | EnCatRacemSimParamWithModel q -> EnCatalyticRacemizationSimilarModel q |> EnCatRacemSimModel
