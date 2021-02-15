namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.LigationModel
open ClmImpure.ReactionRateModels.ActivationModel

module AcFwdCatalyticLigationRandomModel =

    type AcFwdCatalyticLigationRandomParamWithModel =
        {
            acFwdCatLigationParam : AcFwdCatalyticLigationRandomParam
            ligationModel : LigationModel
            activationModel : ActivationModel
        }


    type AcFwdCatalyticLigationRandomModel (p : AcFwdCatalyticLigationRandomParamWithModel) =
        inherit RateModel<AcFwdCatalyticLigationRandomParamWithModel, AcFwdCatalyticLigationReaction>(p)

        let calculateAcCatRatesImpl rnd t (AcFwdCatalyticLigationReaction (s, c)) =
            {
                reaction = s
                acCatalyst = c
                acEeParams = p.acFwdCatLigationParam.acFwdCatLigRndEeParams

                proxy =
                    {
                        getNonActivated = fun e -> e.peptide
                        getCatEnantiomer = getEnantiomer
                        getAcEnantiomer = getEnantiomer
                        acCatReactionCreator = AcFwdCatalyticLigationReaction
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

        member model.getRates t rnd r =
//            printfn $"AcFwdCatalyticLigationRandomModel.getRates: r = {r}, t = {t}."
            getAcRatesImpl model.info (calculateAcCatRatesImpl rnd t) r
