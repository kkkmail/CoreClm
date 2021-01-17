namespace ClmImpure.ReactionRateModels

open ClmImpure.ReactionRateModels.EnCatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticLigationSimilarModel

module EnCatalyticLigationModel =

    type EnCatalyticLigationParamWithModel =
        | EnCatLigRndParamWithModel of EnCatalyticLigationRandomParamWithModel
        | EnCatLigSimParamWithModel of EnCatalyticLigationSimilarParamWithModel


    type EnCatalyticLigationModel =
        | EnCatLigRndModel of EnCatalyticLigationRandomModel
        | EnCatLigSimModel of EnCatalyticLigationSimilarModel

        member model.getRates t rnd r =
            match model with
            | EnCatLigRndModel m -> m.getRates t rnd r
            | EnCatLigSimModel m -> m.getRates t rnd r

        member model.inputParams =
            match model with
            | EnCatLigRndModel m -> m.inputParams |> EnCatLigRndParamWithModel
            | EnCatLigSimModel m -> m.inputParams |> EnCatLigSimParamWithModel

        member model.getAllRates() =
            match model with
            | EnCatLigRndModel m -> m.getAllRates()
            | EnCatLigSimModel m -> m.getAllRates()

        static member create p =
            match p with
            | EnCatLigRndParamWithModel q -> EnCatalyticLigationRandomModel q |> EnCatLigRndModel
            | EnCatLigSimParamWithModel q -> EnCatalyticLigationSimilarModel q |> EnCatLigSimModel
