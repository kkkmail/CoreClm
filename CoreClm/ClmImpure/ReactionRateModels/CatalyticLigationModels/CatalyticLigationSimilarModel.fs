namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRates
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.CatalyticLigationRandomModel

module CatalyticLigationSimilarModel =

    type CatalyticLigationSimilarParamWithModel =
        {
            catLigModel : CatalyticLigationRandomModel
            peptideBondData : PeptideBondData
            catLigSimParam : CatRatesSimilarityParam
        }


    type CatalyticLigationSimilarModel (p : CatalyticLigationSimilarParamWithModel) =
        let calculateSimRatesImpl rnd t (CatalyticLigationReaction (s, c)) =
            let (LigationReaction a) = s
            {
                reaction = s
                catalyst = c
                getReactionData = fun r -> p.peptideBondData.findSameBondSymmetry r.peptideBond
                getMatchingReactionMult = fun x -> x
                inverse = fun r -> r.peptideBond
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticLigationReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = fun e -> p.peptideBondData.findSameBond e
                getBaseRates = p.catLigModel.inputParams.ligationModel.getRates rnd
                getBaseCatRates = p.catLigModel.getRates rnd t
                simParams = p.catLigSimParam
                eeParams = p.catLigModel.inputParams.catLigationParam.catLigRndEeParams
                rateDictionary = p.catLigModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.catLigModel.rateDictionary
