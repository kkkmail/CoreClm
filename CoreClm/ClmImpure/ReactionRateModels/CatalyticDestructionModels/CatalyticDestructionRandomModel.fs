namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.DestructionModel

module CatalyticDestructionRandomModel =

    type CatalyticDestructionRandomParamWithModel =
        {
            catDestrRndParam : CatalyticDestructionRandomParam
            destructionModel : DestructionModel
        }


    type CatalyticDestructionRandomModel (p : CatalyticDestructionRandomParamWithModel) =
        inherit RateModel<CatalyticDestructionRandomParamWithModel, CatalyticDestructionReaction>(p)

        let calculateCatSynthRates rnd t (CatalyticDestructionReaction (s, c)) =
            {
                reaction = s
                catalyst = c
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticDestructionReaction
                getBaseRates = p.destructionModel.getRates rnd
                eeParams = p.catDestrRndParam.catDestrRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateCatRates

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateCatSynthRates rnd t) r
