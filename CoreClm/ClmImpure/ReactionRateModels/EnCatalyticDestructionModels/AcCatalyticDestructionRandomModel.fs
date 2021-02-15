namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.DestructionModel
open ClmImpure.ReactionRateModels.ActivationModel

module AcCatalyticDestructionRandomModel =

    type AcCatalyticDestructionRandomParamWithModel =
        {
            acCatDestrRndParam : AcCatalyticDestructionRandomParam
            destructionModel : DestructionModel
            activationModel : ActivationModel
        }


    type AcCatalyticDestructionRandomModel (p : AcCatalyticDestructionRandomParamWithModel) =
        inherit RateModel<AcCatalyticDestructionRandomParamWithModel, AcCatalyticDestructionReaction>(p)

        let calculateAcCatRatesImpl rnd t (AcCatalyticDestructionReaction (s, c)) =
            {
                reaction = s
                acCatalyst = c
                acEeParams = p.acCatDestrRndParam.acCatDestrRndEeParams

                proxy =
                    {
                        getNonActivated = fun e -> e.peptide
                        getCatEnantiomer = getEnantiomer
                        getAcEnantiomer = getEnantiomer
                        acCatReactionCreator = AcCatalyticDestructionReaction
                        createActivationData = p.activationModel.createActivationData rnd
                        getBaseRates = p.destructionModel.getRates rnd
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

        member model.getRates t rnd r = getAcRatesImpl model.info (calculateAcCatRatesImpl rnd t) r
