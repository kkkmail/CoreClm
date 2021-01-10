namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.LigationModel

module AcFwdCatalyticLigationRandomModel =

    type AcFwdCatalyticLigationRandomParamWithModel =
        {
            acFwdCatLigationParam : AcFwdCatalyticLigationRandomParam
            ligationModel : LigationModel
        }


    type AcFwdCatalyticLigationRandomModel (p : AcFwdCatalyticLigationRandomParamWithModel) =
        inherit RateModel<AcFwdCatalyticLigationRandomParamWithModel, AcFwdCatalyticLigationReaction>(p)

        let calculateCatSynthRates rnd t (AcFwdCatalyticLigationReaction (s, c)) =
            {
                reaction = s
                acCatalyst = c
                getCatEnantiomer = getEnantiomer
                acCatReactionCreator = AcFwdCatalyticLigationReaction
                getBaseRates = p.ligationModel.getRates rnd
                acEeParams = p.acFwdCatLigationParam.acFwdCatLigRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateAcCatRates

        member model.getRates rnd t r = getRatesAllRateDataImpl model.rateDictionary getEnantiomer (calculateCatSynthRates rnd t) r
