namespace ClmImpure.ReactionRateModels

open ClmImpure.ReactionRateModels.AcCatalyticRacemizationRandomModel
open ClmImpure.ReactionRateModels.AcCatalyticRacemizationSimilarModel

module AcCatalyticRacemizationModel =

    type AcCatalyticRacemizationParamWithModel =
        | AcCatRacemRndParamWithModel of AcCatalyticRacemizationRandomParamWithModel
        | AcCatRacemSimParamWithModel of AcCatalyticRacemizationSimilarParamWithModel


    type AcCatalyticRacemizationModel =
        | AcCatRacemRndModel of AcCatalyticRacemizationRandomModel
        | AcCatRacemSimModel of AcCatalyticRacemizationSimilarModel

        member model.getRates rnd t r =
            match model with
            | AcCatRacemRndModel m -> m.getRates rnd t r
            | AcCatRacemSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | AcCatRacemRndModel m -> m.inputParams |> AcCatRacemRndParamWithModel
            | AcCatRacemSimModel m -> m.inputParams |> AcCatRacemSimParamWithModel

        member model.getAllRates() =
            match model with
            | AcCatRacemRndModel m -> m.getAllRates()
            | AcCatRacemSimModel m -> m.getAllRates()

        static member create p =
            match p with
            | AcCatRacemRndParamWithModel q -> AcCatalyticRacemizationRandomModel q |> AcCatRacemRndModel
            | AcCatRacemSimParamWithModel q -> AcCatalyticRacemizationSimilarModel q |> AcCatRacemSimModel
