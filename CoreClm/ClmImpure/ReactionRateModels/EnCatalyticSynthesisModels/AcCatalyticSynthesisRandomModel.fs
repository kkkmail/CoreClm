namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.SynthesisModel
open ClmImpure.ReactionRateModels.ActivationModel

module AcCatalyticSynthesisRandomModel =

    type AcCatalyticSynthesisRandomParamWithModel =
        {
            acCatSynthRndParam : AcCatalyticSynthesisRandomParam
            synthesisModel : SynthesisModel
            activationModel : ActivationModel
        }


    type AcCatalyticSynthesisRandomModel (p : AcCatalyticSynthesisRandomParamWithModel) =
        inherit RateModel<AcCatalyticSynthesisRandomParamWithModel, AcCatalyticSynthesisReaction>(p)

        let calculateAcCatRatesImpl rnd t (AcCatalyticSynthesisReaction (s, c)) =
            {
                reaction = s
                acCatalyst = c
                acEeParams = p.acCatSynthRndParam.acCatSynthRndEeParams

                proxy =
                    {
                        getNonActivated = fun e -> e.peptide
                        getCatEnantiomer = getEnantiomer
                        getAcEnantiomer = getEnantiomer
                        acCatReactionCreator = AcCatalyticSynthesisReaction
                        createActivationData = p.activationModel.createActivationData rnd
                        getBaseRates = p.synthesisModel.getRates rnd
                        rateGenerationType = t
                        rnd = rnd
                    }
            }
            |> calculateAcCatRates

        member private model.info =
            {
                dictionaryData = model.dictionaryData
                acRateDictionary = p.activationModel.dictionaryData.rateDictionary
                getEnantiomer = getEnantiomer
                getAcEnantiomer = getEnantiomer
            }

        member model.getRates t rnd r = getAcRatesImpl model.info (calculateAcCatRatesImpl rnd t) r
