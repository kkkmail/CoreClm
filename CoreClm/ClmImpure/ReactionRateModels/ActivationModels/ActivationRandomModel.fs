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
            let result = getRates (p.forwardScale, d.nextDouble rnd |> Some) (None, None)
            printfn $"ActivationRandomModel.calculateRates: result = %0A{result}."
            result

        let calculateNullRates rnd _ =
            let result = getRates (None, None) (None, None)
            printfn $"ActivationRandomModel.calculateNullRates: result = %0A{result}."
            result

        let getActivationReaction (rnd : RandomValueGetter) p =
            let s =
                match rnd.nextBool() with
                | false -> Ls Z
                | true -> Rs Z

            ActivationReaction (s, p)

        member private model.getRatesInternal rnd r =
            printfn "ActivationRandomModel.getRatesInternal..."
            getRatesImpl model.dictionaryData getEnantiomer (calculateRates rnd) r

        member model.getRates rnd r =
            printfn "ActivationRandomModel.getRates..."
            getRatesImpl model.dictionaryData getEnantiomer (calculateNullRates rnd) r

        /// TODO kk:20210203 - Note that currently this function is 100% enantioselective.
        /// Refer to ActivationRandomParam for the necessary tweaks.
        /// Creates activation reaction(s) for a given peptide and then calculates relevant rates.
        member model.createActivationData (rnd : RandomValueGetter) (p : Peptide) : ReactionRateData<ActivationReaction> =
            let r = getActivationReaction rnd p

            let result =
                {
                    reaction = r
                    rateData = model.getRatesInternal rnd r
                }

            printfn $"ActivationRandomModel.createActivationData: reaction = %0A{result.reaction}, rateData = %0A{result.rateData}."
            result

