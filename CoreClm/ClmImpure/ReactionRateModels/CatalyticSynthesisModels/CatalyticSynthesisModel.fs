﻿namespace ClmImpure.ReactionRateModels

open ClmImpure.ReactionRateModels.CatalyticSynthesisRandomModel
open ClmImpure.ReactionRateModels.CatalyticSynthesisSimilarModel

module CatalyticSynthesisModel =

    type CatalyticSynthesisParamWithModel =
        | CatSynthRndParamWithModel of CatalyticSynthesisRandomParamWithModel
        | CatSynthSimParamWithModel of CatalyticSynthesisSimilarParamWithModel


    type CatalyticSynthesisModel =
        | CatSynthRndModel of CatalyticSynthesisRandomModel
        | CatSynthSimModel of CatalyticSynthesisSimilarModel

        member model.getRates t rnd r =
            match model with
            | CatSynthRndModel m -> m.getRates t rnd r
            | CatSynthSimModel m -> m.getRates t rnd r

        member model.inputParams =
            match model with
            | CatSynthRndModel m -> m.inputParams |> CatSynthRndParamWithModel
            | CatSynthSimModel m -> m.inputParams |> CatSynthSimParamWithModel

        member model.getAllRates() =
            match model with
            | CatSynthRndModel m -> m.getAllRates()
            | CatSynthSimModel m -> m.getAllRates()

        static member create p =
            match p with
            | CatSynthRndParamWithModel q -> CatalyticSynthesisRandomModel q |> CatSynthRndModel
            | CatSynthSimParamWithModel q -> CatalyticSynthesisSimilarModel q |> CatSynthSimModel
