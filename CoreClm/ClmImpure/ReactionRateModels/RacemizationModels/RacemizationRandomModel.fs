namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase

module RacemizationRandomModel =

    type RacemizationRandomModel (p : RacemizationRandomParam) =
        inherit RateModel<RacemizationRandomParam, RacemizationReaction>(p)

        let calculateRates rnd _ =
            let d = p.racemizationDistribution
            getRates (p.forwardScale, d.nextDouble rnd |> Some) (None, None)

        member model.getRates rnd r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd) r
