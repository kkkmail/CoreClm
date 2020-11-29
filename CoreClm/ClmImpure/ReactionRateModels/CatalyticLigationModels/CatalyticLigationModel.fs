namespace ClmImpure.ReactionRateModels

open ClmImpure.ReactionRateModels.CatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.CatalyticLigationSimilarModel

module CatalyticLigationModel =

    type CatalyticLigationParamWithModel =
        | CatLigRndParamWithModel of CatalyticLigationRandomParamWithModel
        | CatLigSimParamWithModel of CatalyticLigationSimilarParamWithModel


    type CatalyticLigationModel =
        | CatLigRndModel of CatalyticLigationRandomModel
        | CatLigSimModel of CatalyticLigationSimilarModel

        member model.getRates rnd t r =
            match model with
            | CatLigRndModel m -> m.getRates rnd t r
            | CatLigSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | CatLigRndModel m -> m.inputParams |> CatLigRndParamWithModel
            | CatLigSimModel m -> m.inputParams |> CatLigSimParamWithModel

        member model.getAllRates() =
            match model with
            | CatLigRndModel m -> m.getAllRates()
            | CatLigSimModel m -> m.getAllRates()

        static member create p =
            match p with
            | CatLigRndParamWithModel q -> CatalyticLigationRandomModel q |> CatLigRndModel
            | CatLigSimParamWithModel q -> CatalyticLigationSimilarModel q |> CatLigSimModel
