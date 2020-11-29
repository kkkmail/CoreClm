namespace ClmImpure.ReactionRateModels

open Clm.ReactionRateParams
open ClmImpure.ReactionRateModels.SugarSynthesisRandomModel

module SugarSynthesisModel =

    type SugarSynthesisModel =
        | SugarSynthRndModel of SugarSynthesisRandomModel

        member model.getRates rnd r =
            match model with
            | SugarSynthRndModel m -> m.getRates rnd r

        member model.inputParams =
            match model with
            | SugarSynthRndModel m -> m.inputParams |> SugarSynthRndParam

        member model.getAllRates() =
            match model with
            | SugarSynthRndModel m -> m.getAllRates()

        static member create p =
            match p with
            | SugarSynthRndParam q -> SugarSynthesisRandomModel q |> SugarSynthRndModel
