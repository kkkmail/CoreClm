namespace ClmImpure.ReactionRateModels

open System.Collections.Generic
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
            dictionaryUpdateType : DictionaryUpdateType
        }


    type CatalyticLigationSimilarModel (p : CatalyticLigationSimilarParamWithModel) =
        let dictionaryData =
            match p.dictionaryUpdateType with
            | AllRateData -> toDictionaryData p.catLigModel.rateDictionary
            | NonOptionalRateDataOnly ->
                {
                    keySetData =
                        {
                            keySet = HashSet<LigCatalyst>()
                            getReactionKey = fun (r : CatalyticLigationReaction) -> r.catalyst
                        }
                        |> Some
                    rateDictionary = p.catLigModel.rateDictionary
                }

        let calculateSimRatesImpl rnd t (CatalyticLigationReaction (s, c)) =
            let (LigationReaction a) = s
            {
                reaction = s
                catalyst = c
                getReactionData = fun r -> p.peptideBondData.findSameBondSymmetry r.peptideBond
                getMatchingReactionMult = id
                inverse = fun r -> r.peptideBond
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticLigationReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = p.peptideBondData.findSameBond
                getBaseRates = p.catLigModel.inputParams.ligationModel.getRates rnd
                getBaseCatRates = p.catLigModel.getRates t rnd
                simParams = p.catLigSimParam
                eeParams = p.catLigModel.inputParams.catLigationParam.catLigRndEeParams
                dictionaryData = dictionaryData
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateSimRates

        member _.getRates t rnd r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.catLigModel.rateDictionary
