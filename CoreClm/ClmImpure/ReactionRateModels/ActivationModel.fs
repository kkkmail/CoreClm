namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase

module ActivationModel =

    type ActivationModel (p : ActivationParam) =
        inherit RateModel<ActivationParam, ActivationReaction>(p)
//        let calculateRates _ = getRates (Some p.foodCreationRate, Some 1.0) (None, None)
//        member model.getRates r = getRatesImpl model.rateDictionary getEnantiomer calculateRates r
        member model.getRates r = failwith ""
