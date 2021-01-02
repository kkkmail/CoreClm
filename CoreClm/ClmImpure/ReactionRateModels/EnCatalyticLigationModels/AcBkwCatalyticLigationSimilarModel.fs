namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.AcBkwCatalyticLigationRandomModel

module AcBkwCatalyticLigationSimilarModel =

    type AcBkwCatalyticLigationSimilarParamWithModel =
        {
            acBkwCatLigModel : AcBkwCatalyticLigationRandomModel
            peptideBondData : PeptideBondData
            acBkwCatLigSimParam : AcCatRatesSimilarityParam
        }


    type AcBkwCatalyticLigationSimilarModel (p : AcBkwCatalyticLigationSimilarParamWithModel) =
        let calculateSimRatesImpl rnd t (AcBkwCatalyticLigationReaction (s, c)) =
            {
                reaction = s
                acCatalyst = c
                getReactionData = fun r -> p.peptideBondData.findSameBondSymmetry r.peptideBond
                getMatchingReactionMult = fun x -> x
                inverse = fun r -> r.peptideBond
                getCatEnantiomer = getEnantiomer
                acCatReactionCreator = AcBkwCatalyticLigationReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = fun e -> p.peptideBondData.findSameBond e
                getBaseRates = p.acBkwCatLigModel.inputParams.ligationModel.getRates rnd
                getBaseCatRates = p.acBkwCatLigModel.getRates rnd t
                acSimParams = p.acBkwCatLigSimParam
                acEeParams = p.acBkwCatLigModel.inputParams.acBkwCatLigationParam.acBkwCatLigRndEeParams
                rateDictionary = p.acBkwCatLigModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateAcSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.acBkwCatLigModel.rateDictionary
