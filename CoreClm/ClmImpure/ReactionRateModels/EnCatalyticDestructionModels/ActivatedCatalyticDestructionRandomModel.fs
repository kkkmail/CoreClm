namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.DestructionModel

module ActivatedCatalyticDestructionRandomModel =

    type ActivatedCatalyticDestructionRandomParamWithModel =
        {
            enCatDestrRndParam : ActivatedCatalyticDestructionRandomParam
            destructionModel : DestructionModel
        }


    type ActivatedCatalyticDestructionRandomModel (p : ActivatedCatalyticDestructionRandomParamWithModel) =
        inherit RateModel<ActivatedCatalyticDestructionRandomParamWithModel, ActivatedCatalyticDestructionReaction>(p)

        let calculateActivatedCatDestrRates rnd t (ActivatedCatalyticDestructionReaction (s, c)) =
            {
                reaction = s
                acCatalyst = c
                getCatEnantiomer = getEnantiomer
                acCatReactionCreator = ActivatedCatalyticDestructionReaction
                getBaseRates = p.destructionModel.getRates rnd
                eeParams = p.enCatDestrRndParam.acCatDestrRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateActivatedCatRates

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateActivatedCatDestrRates rnd t) r
