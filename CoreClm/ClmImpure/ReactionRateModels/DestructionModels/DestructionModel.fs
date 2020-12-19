namespace ClmImpure.ReactionRateModels

open Clm.ReactionRateParams
open ClmImpure.ReactionRateModels.DestructionRandomModel

module DestructionModel =

    type DestructionModel =
        | DestrRndModel of DestructionRandomModel

        member model.getRates rnd r =
            match model with
            | DestrRndModel m -> m.getRates rnd r

        member model.inputParams =
            match model with
            | DestrRndModel m -> m.inputParams |> DestrRndParam

        member model.getAllRates() =
            match model with
            | DestrRndModel m -> m.getAllRates()

        static member create p =
            match p with
            | DestrRndParam q -> DestructionRandomModel q |> DestrRndModel
