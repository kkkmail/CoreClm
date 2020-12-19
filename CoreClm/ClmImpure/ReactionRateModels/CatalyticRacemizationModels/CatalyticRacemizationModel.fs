namespace ClmImpure.ReactionRateModels

open ClmImpure.ReactionRateModels.CatalyticRacemizationRandomModel
open ClmImpure.ReactionRateModels.CatalyticRacemizationSimilarModel

module CatalyticRacemizationModel =

    type CatalyticRacemizationParamWithModel =
        | CatRacemRndParamWithModel of CatalyticRacemizationRandomParamWithModel
        | CatRacemSimParamWithModel of CatalyticRacemizationSimilarParamWithModel


    type CatalyticRacemizationModel =
        | CatRacemRndModel of CatalyticRacemizationRandomModel
        | CatRacemSimModel of CatalyticRacemizationSimilarModel

        member model.getRates rnd t r =
            match model with
            | CatRacemRndModel m -> m.getRates rnd t r
            | CatRacemSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | CatRacemRndModel m -> m.inputParams |> CatRacemRndParamWithModel
            | CatRacemSimModel m -> m.inputParams |> CatRacemSimParamWithModel

        member model.getAllRates() =
            match model with
            | CatRacemRndModel m -> m.getAllRates()
            | CatRacemSimModel m -> m.getAllRates()

        static member create p =
            match p with
            | CatRacemRndParamWithModel q -> CatalyticRacemizationRandomModel q |> CatRacemRndModel
            | CatRacemSimParamWithModel q -> CatalyticRacemizationSimilarModel q |> CatRacemSimModel
