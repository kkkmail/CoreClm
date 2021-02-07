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

        let calculateRates rnd r =
            let i =
                {
                    activationParam = p
                    rnd = rnd
                }

            let result = calculateActivationRates i r

//            let d = p.activationDistribution
//            let result = getRates (p.forwardScale, d.nextDouble rnd |> Some) (None, None)
            printfn $"ActivationRandomModel.calculateRates: r = {r}, result = %0A{result}."
            result

        let calculateNullRates rnd _ =
            let result = getRates (None, None) (None, None)
//            printfn $"ActivationRandomModel.calculateNullRates: result = %0A{result}."
            result

        let getActivationReaction (rnd : RandomValueGetter) p =
            let s =
                match rnd.nextBool() with
                | false -> Ls Z
                | true -> Rs Z

            let result = ActivationReaction (s, p)
            printfn $"getActivationReaction: result = {result}"
            result

        member private model.getRatesInternal rnd r =
            printfn $"ActivationRandomModel.getRatesInternal: Starting..."
            let result = getRatesImpl model.dictionaryData getEnantiomer (calculateRates rnd) r
            printfn $"ActivationRandomModel.getRatesInternal: r = {r}, result = {result}"
            result

        /// Activation reactions are different because we calculate them only when needed.
        /// As such the primary generation route is "disabled".
        member model.getRates rnd r =
//            printfn "ActivationRandomModel.getRates..."
            getRatesImpl model.dictionaryData getEnantiomer (calculateNullRates rnd) r

        /// Creates activation reaction(s) for a given peptide and then calculates relevant rates.
        member model.createActivationData (rnd : RandomValueGetter) (p : Peptide) : ReactionRateData<ActivationReaction> =
            let r = getActivationReaction rnd p

            let result =
                {
                    reaction = r
                    rateData = model.getRatesInternal rnd r
                }

            printfn $"ActivationRandomModel.createActivationData: reaction = {result.reaction}, rateData = %0A{result.rateData}."
            result

