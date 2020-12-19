namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase

module SugarSynthesisRandomModel =

    type SugarSynthesisRandomModel (p : SugarSynthesisRandomParam) =
        inherit RateModel<SugarSynthesisRandomParam, SugarSynthesisReaction>(p)

        let calculateRates rnd _ =
            let d = p.sugarSynthesisDistribution
            getRates (p.forwardScale, d.nextDouble rnd |> Some) (p.backwardScale, d.nextDouble rnd |> Some)

        member model.getRates rnd r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd) r
