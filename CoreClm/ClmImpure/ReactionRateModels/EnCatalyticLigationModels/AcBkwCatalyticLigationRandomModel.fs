namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.LigationModel

module AcBkwCatalyticLigationRandomModel =

    type AcBkwCatalyticLigationRandomParamWithModel =
        {
            acBkwCatLigationParam : AcBkwCatalyticLigationRandomParam
            ligationModel : LigationModel
        }


    type AcBkwCatalyticLigationRandomModel (p : AcBkwCatalyticLigationRandomParamWithModel) =
        inherit RateModel<AcBkwCatalyticLigationRandomParamWithModel, AcBkwCatalyticLigationReaction>(p)

        let calculateCatSynthRates rnd t (AcBkwCatalyticLigationReaction (s, c)) =
            {
                reaction = s
                acCatalyst = c
                getCatEnantiomer = getEnantiomer
                acCatReactionCreator = AcBkwCatalyticLigationReaction
                getBaseRates = p.ligationModel.getRates rnd
                acEeParams = p.acBkwCatLigationParam.acBkwCatLigRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateAcCatRates

        member model.getRates rnd t r = getRatesAllRateDataImpl model.rateDictionary getEnantiomer (calculateCatSynthRates rnd t) r
