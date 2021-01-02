namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.DestructionModel

module AcCatalyticDestructionRandomModel =

    type AcCatalyticDestructionRandomParamWithModel =
        {
            acCatDestrRndParam : AcCatalyticDestructionRandomParam
            destructionModel : DestructionModel
        }


    type AcCatalyticDestructionRandomModel (p : AcCatalyticDestructionRandomParamWithModel) =
        inherit RateModel<AcCatalyticDestructionRandomParamWithModel, AcCatalyticDestructionReaction>(p)

        let calculateAcCatDestrRates rnd t (AcCatalyticDestructionReaction (s, c)) =
            {
                reaction = s
                acCatalyst = c
                getCatEnantiomer = getEnantiomer
                acCatReactionCreator = AcCatalyticDestructionReaction
                getBaseRates = p.destructionModel.getRates rnd
                acEeParams = p.acCatDestrRndParam.acCatDestrRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateAcCatRates

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateAcCatDestrRates rnd t) r
