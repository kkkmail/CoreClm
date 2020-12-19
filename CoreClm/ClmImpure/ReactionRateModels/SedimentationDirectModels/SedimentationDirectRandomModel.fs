namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase

module SedimentationDirectRandomModel =

    type SedimentationDirectRandomModel (p : SedimentationDirectRandomParam) =
        inherit RateModel<SedimentationDirectRandomParam, SedimentationDirectReaction>(p)

        let calculateRates rnd t _ =
            let k =
                match t with
                | RandomChoice -> p.sedDirDistribution.nextDouble rnd |> Some
            getForwardRates (p.forwardScale, k)

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd t) r
