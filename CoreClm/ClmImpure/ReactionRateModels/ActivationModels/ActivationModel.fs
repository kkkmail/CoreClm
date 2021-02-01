namespace ClmImpure.ReactionRateModels

open Clm.ReactionRateParams
open ClmImpure.ReactionRateModels.ActivationRandomModel

module ActivationModel =

    type ActivationModel =
        | ActivationRndModel of ActivationRandomModel

        member model.getRates rnd r =
            match model with
            | ActivationRndModel m -> m.getRates rnd r

        /// Creates activation reaction(s) for a given peptide and then calculates relevant rates.
        member model.createActivationData rnd p =
            match model with
            | ActivationRndModel m -> m.createActivationData rnd p

        member model.inputParams =
            match model with
            | ActivationRndModel m -> m.inputParams |> ActivationRndParam

        member model.dictionaryData =
            match model with
            | ActivationRndModel m -> m.dictionaryData

        member model.getAllRates() =
            match model with
            | ActivationRndModel m -> m.getAllRates()

        static member create p =
            match p with
            | ActivationRndParam q -> ActivationRandomModel q |> ActivationRndModel
