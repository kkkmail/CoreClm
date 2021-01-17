namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.RacemizationModel

module CatalyticRacemizationRandomModel =

    type CatalyticRacemizationRandomParamWithModel =
        {
            catRacemRndParam : CatalyticRacemizationRandomParam
            racemizationModel : RacemizationModel
            aminoAcids : list<AminoAcid>
        }


    type CatalyticRacemizationRandomModel (p : CatalyticRacemizationRandomParamWithModel) =
        inherit RateModel<CatalyticRacemizationRandomParamWithModel, CatalyticRacemizationReaction>(p)

        let calculateCatSynthRates rnd t (CatalyticRacemizationReaction (s, c)) =
            {
                reaction = s
                catalyst = c
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticRacemizationReaction
                getBaseRates = p.racemizationModel.getRates rnd
                eeParams = p.catRacemRndParam.catRacemRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateCatRates

        member model.getRates t rnd r = getRatesImpl model.dictionaryData getEnantiomer (calculateCatSynthRates rnd t) r
