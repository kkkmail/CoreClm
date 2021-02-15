namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.RacemizationModel
open ClmImpure.ReactionRateModels.ActivationModel

module AcCatalyticRacemizationRandomModel =

    type AcCatalyticRacemizationRandomParamWithModel =
        {
            acCatRacemRndParam : AcCatalyticRacemizationRandomParam
            racemizationModel : RacemizationModel
            activationModel : ActivationModel
        }


    type AcCatalyticRacemizationRandomModel (p : AcCatalyticRacemizationRandomParamWithModel) =
        inherit RateModel<AcCatalyticRacemizationRandomParamWithModel, AcCatalyticRacemizationReaction>(p)

        let calculateAcCatRatesImpl rnd t (AcCatalyticRacemizationReaction (s, c)) =
            {
                reaction = s
                acCatalyst = c
                acEeParams = p.acCatRacemRndParam.acCatRacemRndEeParams

                proxy =
                    {
                        getNonActivated = fun e -> e.peptide
                        getCatEnantiomer = getEnantiomer
                        getAcEnantiomer = getEnantiomer
                        acCatReactionCreator = AcCatalyticRacemizationReaction
                        createActivationData = p.activationModel.createActivationData rnd
                        getBaseRates = p.racemizationModel.getRates rnd
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
