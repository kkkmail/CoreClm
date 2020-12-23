namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.SynthesisModel

module ActivatedCatalyticSynthesisRandomModel =

    type ActivatedCatalyticSynthesisRandomParamWithModel =
        {
            acCatSynthRndParam : ActivatedCatalyticSynthesisRandomParam
            synthesisModel : SynthesisModel
        }


    type ActivatedCatalyticSynthesisRandomModel (p : ActivatedCatalyticSynthesisRandomParamWithModel) =
        inherit RateModel<ActivatedCatalyticSynthesisRandomParamWithModel, ActivatedCatalyticSynthesisReaction>(p)

        let calculateActivatedCatSynthRates rnd t (ActivatedCatalyticSynthesisReaction (s, c)) =
            {
                reaction = s
                enCatalyst = c
                energySource = u
                getCatEnantiomer = getEnantiomer
                getEnergySourceEnantiomer = getEnantiomer
                enCatReactionCreator = ActivatedCatalyticSynthesisReaction
                getBaseRates = p.synthesisModel.getRates rnd
                eeParams = p.acCatSynthRndParam.enCatSynthRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateActivatedCatRates

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateActivatedCatSynthRates rnd t) r
