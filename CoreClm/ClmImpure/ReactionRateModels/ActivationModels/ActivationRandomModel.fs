namespace ClmImpure.ReactionRateModels

open Clm.Distributions
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

        let getActivationReaction (rnd : RandomValueGetter) p =
            let s =
                match rnd.nextBool() with
                | false -> Ls Z
                | true -> Rs Z

            ActivationReaction (s, p)

        member model.getRates rnd r = getRatesImpl model.dictionaryData getEnantiomer (calculateRates rnd) r

        /// TODO kk:20210203 - Note that currently this function is 100% enantioselective.
        /// Refer to ActivationRandomParam for the necessary tweaks.
        /// Creates activation reaction(s) for a given peptide and then calculates relevant rates.
        member model.createActivationData (rnd : RandomValueGetter) (p : Peptide) : ReactionRateData<ActivationReaction> =
            let r = getActivationReaction rnd p

            {
                reaction = r
                rateData = model.getRates rnd r
            }

