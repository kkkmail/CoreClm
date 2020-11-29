namespace ClmImpure.ReactionRateModels

open Clm.ReactionRateParams
open ClmImpure.ReactionRateModels.SynthesisRandomModel

module SynthesisModel =

    type SynthesisModel =
        | SynthRndModel of SynthesisRandomModel

        member model.getRates rnd r =
            match model with
            | SynthRndModel m -> m.getRates rnd r

        member model.inputParams =
            match model with
            | SynthRndModel m -> m.inputParams |> SynthRndParam

        member model.getAllRates() =
            match model with
            | SynthRndModel m -> m.getAllRates()

        static member create p =
            match p with
            | SynthRndParam q -> SynthesisRandomModel q |> SynthRndModel
