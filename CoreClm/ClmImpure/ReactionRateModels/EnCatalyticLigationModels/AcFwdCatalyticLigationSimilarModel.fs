namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.AcFwdCatalyticLigationRandomModel

module AcFwdCatalyticLigationSimilarModel =

    type AcFwdCatalyticLigationSimilarParamWithModel =
        {
            acFwdCatLigModel : AcFwdCatalyticLigationRandomModel
            peptideBondData : PeptideBondData
            acFwdCatLigSimParam : AcCatRatesSimilarityParam
        }


    type AcFwdCatalyticLigationSimilarModel (p : AcFwdCatalyticLigationSimilarParamWithModel) =
        let calculateSimRatesImpl rnd t (AcFwdCatalyticLigationReaction (s, c)) =
            {
                reaction = s
                acCatalyst = c
                getReactionData = fun r -> p.peptideBondData.findSameBondSymmetry r.peptideBond
                getMatchingReactionMult = fun x -> x
                inverse = fun r -> r.peptideBond
                getCatEnantiomer = getEnantiomer
                acCatReactionCreator = AcFwdCatalyticLigationReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = fun e -> p.peptideBondData.findSameBond e
                getBaseRates = p.acFwdCatLigModel.inputParams.ligationModel.getRates rnd
                getBaseCatRates = p.acFwdCatLigModel.getRates rnd t
                acSimParams = p.acFwdCatLigSimParam
                acEeParams = p.acFwdCatLigModel.inputParams.acFwdCatLigationParam.acFwdCatLigRndEeParams
                rateDictionary = p.acFwdCatLigModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
                dictionaryUpdateType = DictionaryUpdateType.getAcFwdCatDefaultValue()
            }
            |> calculateAcSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.acFwdCatLigModel.rateDictionary
