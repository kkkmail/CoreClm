namespace ClmImpure.ReactionRateModels

open Clm.ReactionRateParams
open ClmImpure.ReactionRateModels.SedimentationAllRandomModel

module SedimentationAllModel =

    type SedimentationAllModel =
        | SedAllRndModel of SedimentationAllRandomModel

        member model.getRates rnd r =
            match model with
            | SedAllRndModel m -> m.getRates rnd r

        member model.inputParams =
            match model with
            | SedAllRndModel m -> m.inputParams |> SedAllRndParam

        member model.getAllRates() =
            match model with
            | SedAllRndModel m -> m.getAllRates()

        static member create p =
            match p with
            | SedAllRndParam q -> SedimentationAllRandomModel q |> SedAllRndModel
