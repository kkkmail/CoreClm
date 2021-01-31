namespace ClmImpure.ReactionRateModels

open Clm.ReactionRateParams
open ClmImpure.ReactionRateModels.ActivationRandomModel

module ActivationModel =

    type ActivationModel =
        | ActivationRndModel of ActivationRandomModel

        member model.getRates rnd r =
            match model with
            | ActivationRndModel m -> m.getRates rnd r

        member model.inputParams =
            match model with
            | ActivationRndModel m -> m.inputParams |> ActivationRndParam

        member model.getAllRates() =
            match model with
            | ActivationRndModel m -> m.getAllRates()

        static member create p =
            match p with
            | ActivationRndParam q -> ActivationRandomModel q |> ActivationRndModel
