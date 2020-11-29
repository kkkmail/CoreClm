namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.RacemizationModel

module EnCatalyticRacemizationRandomModel =

    type EnCatalyticRacemizationRandomParamWithModel =
        {
            enCatRacemRndParam : EnCatalyticRacemizationRandomParam
            racemizationModel : RacemizationModel
        }


    type EnCatalyticRacemizationRandomModel (p : EnCatalyticRacemizationRandomParamWithModel) =
        inherit RateModel<EnCatalyticRacemizationRandomParamWithModel, EnCatalyticRacemizationReaction>(p)

        let calculateEnCatRacemRates rnd t (EnCatalyticRacemizationReaction (s, c, u)) =
            {
                reaction = s
                enCatalyst = c
                energySource = u
                getCatEnantiomer = getEnantiomer
                getEnergySourceEnantiomer = getEnantiomer
                enCatReactionCreator = EnCatalyticRacemizationReaction
                getBaseRates = p.racemizationModel.getRates rnd
                eeParams = p.enCatRacemRndParam.enCatRacemRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateEnCatRates

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateEnCatRacemRates rnd t) r
