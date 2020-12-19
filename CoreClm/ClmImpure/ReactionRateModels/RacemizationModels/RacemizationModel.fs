namespace ClmImpure.ReactionRateModels

open Clm.ReactionRateParams
open ClmImpure.ReactionRateModels.RacemizationRandomModel

module RacemizationModel =

    type RacemizationModel =
        | RacemRndModel of RacemizationRandomModel

        member model.getRates rnd r =
            match model with
            | RacemRndModel m -> m.getRates rnd r

        member model.inputParams =
            match model with
            | RacemRndModel m -> m.inputParams |> RacemRndParam

        member model.getAllRates() =
            match model with
            | RacemRndModel m -> m.getAllRates()

        static member create p =
            match p with
            | RacemRndParam q -> RacemizationRandomModel q |> RacemRndModel
