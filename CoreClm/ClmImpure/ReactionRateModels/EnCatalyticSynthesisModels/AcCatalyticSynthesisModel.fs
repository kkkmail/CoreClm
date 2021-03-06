namespace ClmImpure.ReactionRateModels

open ClmImpure.ReactionRateModels.AcCatalyticSynthesisRandomModel
open ClmImpure.ReactionRateModels.AcCatalyticSynthesisSimilarModel

module AcCatalyticSynthesisModel =

    type AcCatalyticSynthesisParamWithModel =
        | AcCatSynthRndParamWithModel of AcCatalyticSynthesisRandomParamWithModel
        | AcCatSynthSimParamWithModel of AcCatalyticSynthesisSimilarParamWithModel


    type AcCatalyticSynthesisModel =
        | AcCatSynthRndModel of AcCatalyticSynthesisRandomModel
        | AcCatSynthSimModel of AcCatalyticSynthesisSimilarModel

        member model.getRates t rnd r =
            match model with
            | AcCatSynthRndModel m -> m.getRates t rnd r
            | AcCatSynthSimModel m -> m.getRates t rnd r

        member model.inputParams =
            match model with
            | AcCatSynthRndModel m -> m.inputParams |> AcCatSynthRndParamWithModel
            | AcCatSynthSimModel m -> m.inputParams |> AcCatSynthSimParamWithModel

        member model.getAllRates() =
            match model with
            | AcCatSynthRndModel m -> m.getAllRates()
            | AcCatSynthSimModel m -> m.getAllRates()

        static member create p =
            match p with
            | AcCatSynthRndParamWithModel q -> AcCatalyticSynthesisRandomModel q |> AcCatSynthRndModel
            | AcCatSynthSimParamWithModel q -> AcCatalyticSynthesisSimilarModel q |> AcCatSynthSimModel
