namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.RacemizationModel

module AcCatalyticRacemizationRandomModel =

    type AcCatalyticRacemizationRandomParamWithModel =
        {
            acCatRacemRndParam : AcCatalyticRacemizationRandomParam
            racemizationModel : RacemizationModel
        }


    type AcCatalyticRacemizationRandomModel (p : AcCatalyticRacemizationRandomParamWithModel) =
        inherit RateModel<AcCatalyticRacemizationRandomParamWithModel, AcCatalyticRacemizationReaction>(p)

        let calculateAcCatRacemRates rnd t (AcCatalyticRacemizationReaction (s, c)) =
            {
                reaction = s
                acCatalyst = c
                getCatEnantiomer = getEnantiomer
                acCatReactionCreator = AcCatalyticRacemizationReaction
                getBaseRates = p.racemizationModel.getRates rnd
                eeParams = p.acCatRacemRndParam.acCatRacemRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateAcCatRates

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateAcCatRacemRates rnd t) r
