namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.DestructionModel

module EnCatalyticDestructionRandomModel =

    type EnCatalyticDestructionRandomParamWithModel =
        {
            enCatDestrRndParam : EnCatalyticDestructionRandomParam
            destructionModel : DestructionModel
        }


    type EnCatalyticDestructionRandomModel (p : EnCatalyticDestructionRandomParamWithModel) =
        inherit RateModel<EnCatalyticDestructionRandomParamWithModel, EnCatalyticDestructionReaction>(p)

        let calculateEnCatDestrRates rnd t (EnCatalyticDestructionReaction (s, c, u)) =
            {
                reaction = s
                enCatalyst = c
                energySource = u
                getCatEnantiomer = getEnantiomer
                getEnergySourceEnantiomer = getEnantiomer
                enCatReactionCreator = EnCatalyticDestructionReaction
                getBaseRates = p.destructionModel.getRates rnd
                eeParams = p.enCatDestrRndParam.enCatDestrRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateEnCatRates

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateEnCatDestrRates rnd t) r
