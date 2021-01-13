namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.SynthesisModel

module AcCatalyticSynthesisRandomModel =

    type AcCatalyticSynthesisRandomParamWithModel =
        {
            acCatSynthRndParam : AcCatalyticSynthesisRandomParam
            synthesisModel : SynthesisModel
        }


    type AcCatalyticSynthesisRandomModel (p : AcCatalyticSynthesisRandomParamWithModel) =
        inherit RateModel<AcCatalyticSynthesisRandomParamWithModel, AcCatalyticSynthesisReaction>(p)

        let calculateAcCatSynthRates rnd t (AcCatalyticSynthesisReaction (s, c)) =
            {
                reaction = s
                acCatalyst = c
                getCatEnantiomer = getEnantiomer
                acCatReactionCreator = AcCatalyticSynthesisReaction
                getBaseRates = p.synthesisModel.getRates rnd
                acEeParams = p.acCatSynthRndParam.acCatSynthRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateAcCatRates

        member model.getRates rnd t r = getRatesImpl model.dictionaryData getEnantiomer (calculateAcCatSynthRates rnd t) r
