namespace ClmImpure.ReactionRateModels

open System.Collections.Generic
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
            dictionaryUpdateType : DictionaryUpdateType
        }


    type AcBkwCatalyticLigationSimilarModel (p : AcBkwCatalyticLigationSimilarParamWithModel) =
        let dictionaryData =
            match p.dictionaryUpdateType with
            | AllRateData -> toDictionaryData p.acBkwCatLigModel.rateDictionary
            | NonOptionalRateDataOnly ->
                {
                    keySetData =
                        {
                            keySet = HashSet<AcBkwLigCatalyst>()
                            getReactionKey = fun (r : AcBkwCatalyticLigationReaction) -> r.catalyst
                        }
                        |> Some
                    rateDictionary = p.acBkwCatLigModel.rateDictionary
                }

        let calculateSimRatesImpl rnd t (AcBkwCatalyticLigationReaction (s, c)) =
            {
                reaction = s
                acCatalyst = c
                getReactionData = fun r -> p.peptideBondData.findSameBondSymmetry r.peptideBond
                getMatchingReactionMult = id
                inverse = fun r -> r.peptideBond
                getCatEnantiomer = getEnantiomer
                acCatReactionCreator = AcBkwCatalyticLigationReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = p.peptideBondData.findSameBond
                getBaseRates = p.acBkwCatLigModel.inputParams.ligationModel.getRates rnd
                getBaseCatRates = p.acBkwCatLigModel.getRates t
                tryGetBaseCatRates = p.acBkwCatLigModel.tryGetRates
                acSimParams = p.acBkwCatLigSimParam
                acEeParams = p.acBkwCatLigModel.inputParams.acBkwCatLigationParam.acBkwCatLigRndEeParams
                dictionaryData = dictionaryData
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateAcSimRates

        member _.getRates t rnd r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.acBkwCatLigModel.rateDictionary
