namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase

module SedimentationAllRandomModel =

    type SedimentationAllRandomModel (p : SedimentationAllRandomParam) =
        inherit RateModel<SedimentationAllRandomParam, SedimentationAllReaction>(p)
        let calculateRates rnd _ = getForwardRates (p.forwardScale, p.sedimentationAllDistribution.nextDouble rnd |> Some)
        member model.getRates rnd r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd) r

