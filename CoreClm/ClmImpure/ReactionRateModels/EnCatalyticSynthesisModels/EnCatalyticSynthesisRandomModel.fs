namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.SynthesisModel

module EnCatalyticSynthesisRandomModel =

    type EnCatalyticSynthesisRandomParamWithModel =
        {
            enCatSynthRndParam : EnCatalyticSynthesisRandomParam
            synthesisModel : SynthesisModel
        }


    type EnCatalyticSynthesisRandomModel (p : EnCatalyticSynthesisRandomParamWithModel) =
        inherit RateModel<EnCatalyticSynthesisRandomParamWithModel, EnCatalyticSynthesisReaction>(p)

        let calculateEnCatSynthRates rnd t (EnCatalyticSynthesisReaction (s, c, u)) =
            {
                reaction = s
                enCatalyst = c
                energySource = u
                getCatEnantiomer = getEnantiomer
                getEnergySourceEnantiomer = getEnantiomer
                enCatReactionCreator = EnCatalyticSynthesisReaction
                getBaseRates = p.synthesisModel.getRates rnd
                eeParams = p.enCatSynthRndParam.enCatSynthRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateEnCatRates

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateEnCatSynthRates rnd t) r
