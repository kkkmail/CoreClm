﻿namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.LigationModel

module CatalyticLigationRandomModel =

    type CatalyticLigationRandomParamWithModel =
        {
            catLigationParam : CatalyticLigationRandomParam
            ligationModel : LigationModel
        }


    type CatalyticLigationRandomModel (p : CatalyticLigationRandomParamWithModel) =
        inherit RateModel<CatalyticLigationRandomParamWithModel, CatalyticLigationReaction>(p)

        let calculateCatSynthRates rnd t (CatalyticLigationReaction (s, c)) =
            {
                reaction = s
                catalyst = c
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticLigationReaction
                getBaseRates = p.ligationModel.getRates rnd
                eeParams = p.catLigationParam.catLigRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateCatRates

        member model.getRates t rnd r = getRatesImpl model.dictionaryData getEnantiomer (calculateCatSynthRates rnd t) r
