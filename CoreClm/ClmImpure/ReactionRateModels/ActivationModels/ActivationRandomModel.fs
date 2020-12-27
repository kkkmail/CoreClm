namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase

module ActivationRandomModel =

    type ActivationRandomModel (p : ActivationRandomParam) =
        inherit RateModel<ActivationRandomParam, ActivationReaction>(p)

        let calculateRates rnd _ =
            let d = p.activationDistribution
            getRates (p.forwardScale, d.nextDouble rnd |> Some) (None, None)

        member model.getRates rnd r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd) r
