namespace ClmImpure.ReactionRateModels

open ClmImpure.ReactionRateModels.ActivatedCatalyticSynthesisRandomModel
open ClmImpure.ReactionRateModels.ActivatedCatalyticSynthesisSimilarModel

module ActivatedCatalyticSynthesisModel =

    type ActivatedCatalyticSynthesisParamWithModel =
        | ActivatedCatSynthRndParamWithModel of ActivatedCatalyticSynthesisRandomParamWithModel
        | ActivatedCatSynthSimParamWithModel of ActivatedCatalyticSynthesisSimilarParamWithModel


    type ActivatedCatalyticSynthesisModel =
        | ActivatedCatSynthRndModel of ActivatedCatalyticSynthesisRandomModel
        | ActivatedCatSynthSimModel of ActivatedCatalyticSynthesisSimilarModel

        member model.getRates rnd t r =
            match model with
            | ActivatedCatSynthRndModel m -> m.getRates rnd t r
            | ActivatedCatSynthSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | ActivatedCatSynthRndModel m -> m.inputParams |> ActivatedCatSynthRndParamWithModel
            | ActivatedCatSynthSimModel m -> m.inputParams |> ActivatedCatSynthSimParamWithModel

        member model.getAllRates() =
            match model with
            | ActivatedCatSynthRndModel m -> m.getAllRates()
            | ActivatedCatSynthSimModel m -> m.getAllRates()

        static member create p =
            match p with
            | ActivatedCatSynthRndParamWithModel q -> ActivatedCatalyticSynthesisRandomModel q |> ActivatedCatSynthRndModel
            | ActivatedCatSynthSimParamWithModel q -> ActivatedCatalyticSynthesisSimilarModel q |> ActivatedCatSynthSimModel
