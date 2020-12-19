namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.LigationModel

module EnCatalyticLigationRandomModel =

    type EnCatalyticLigationRandomParamWithModel =
        {
            enCatLigationParam : EnCatalyticLigationRandomParam
            ligationModel : LigationModel
        }


    type EnCatalyticLigationRandomModel (p : EnCatalyticLigationRandomParamWithModel) =
        inherit RateModel<EnCatalyticLigationRandomParamWithModel, EnCatalyticLigationReaction>(p)

        let calculateCatSynthRates rnd t (EnCatalyticLigationReaction (s, c, u)) =
            {
                reaction = s
                enCatalyst = c
                energySource = u
                getCatEnantiomer = getEnantiomer
                getEnergySourceEnantiomer = getEnantiomer
                enCatReactionCreator = EnCatalyticLigationReaction
                getBaseRates = p.ligationModel.getRates rnd
                eeParams = p.enCatLigationParam.enCatLigRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateEnCatRates

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateCatSynthRates rnd t) r
