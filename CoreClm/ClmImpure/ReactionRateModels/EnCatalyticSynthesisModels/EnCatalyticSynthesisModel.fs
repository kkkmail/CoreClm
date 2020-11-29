namespace ClmImpure.ReactionRateModels

open ClmImpure.ReactionRateModels.EnCatalyticSynthesisRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticSynthesisSimilarModel

module EnCatalyticSynthesisModel =

    type EnCatalyticSynthesisParamWithModel =
        | EnCatSynthRndParamWithModel of EnCatalyticSynthesisRandomParamWithModel
        | EnCatSynthSimParamWithModel of EnCatalyticSynthesisSimilarParamWithModel


    type EnCatalyticSynthesisModel =
        | EnCatSynthRndModel of EnCatalyticSynthesisRandomModel
        | EnCatSynthSimModel of EnCatalyticSynthesisSimilarModel

        member model.getRates rnd t r =
            match model with
            | EnCatSynthRndModel m -> m.getRates rnd t r
            | EnCatSynthSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | EnCatSynthRndModel m -> m.inputParams |> EnCatSynthRndParamWithModel
            | EnCatSynthSimModel m -> m.inputParams |> EnCatSynthSimParamWithModel

        member model.getAllRates() =
            match model with
            | EnCatSynthRndModel m -> m.getAllRates()
            | EnCatSynthSimModel m -> m.getAllRates()

        static member create p =
            match p with
            | EnCatSynthRndParamWithModel q -> EnCatalyticSynthesisRandomModel q |> EnCatSynthRndModel
            | EnCatSynthSimParamWithModel q -> EnCatalyticSynthesisSimilarModel q |> EnCatSynthSimModel
