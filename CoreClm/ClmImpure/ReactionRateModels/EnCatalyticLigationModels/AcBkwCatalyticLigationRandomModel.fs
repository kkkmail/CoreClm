namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.LigationModel
open ClmImpure.ReactionRateModels.ActivationModel

module AcBkwCatalyticLigationRandomModel =

    type AcBkwCatalyticLigationRandomParamWithModel =
        {
            acBkwCatLigationParam : AcBkwCatalyticLigationRandomParam
            ligationModel : LigationModel
            activationModel : ActivationModel
        }


    type AcBkwCatalyticLigationRandomModel (p : AcBkwCatalyticLigationRandomParamWithModel) =
        inherit RateModel<AcBkwCatalyticLigationRandomParamWithModel, AcBkwCatalyticLigationReaction>(p)

        let calculateAcCatRatesImpl rnd t (AcBkwCatalyticLigationReaction (s, c)) =
            {
                reaction = s
                acCatalyst = c
                acEeParams = p.acBkwCatLigationParam.acBkwCatLigRndEeParams

                proxy =
                    {
                        getNonActivated = fun e -> e.peptide
                        getCatEnantiomer = getEnantiomer
                        getAcEnantiomer = getEnantiomer
                        acCatReactionCreator = AcBkwCatalyticLigationReaction
                        createActivationData = p.activationModel.createActivationData rnd
                        getBaseRates = p.ligationModel.getRates rnd
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
