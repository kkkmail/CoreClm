namespace ClmImpure.ReactionRateModels

open System.Collections.Generic
open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.EnCatalyticLigationRandomModel

module EnCatalyticLigationSimilarModel =

    type EnCatalyticLigationSimilarParamWithModel =
        {
            enCatLigModel : EnCatalyticLigationRandomModel
            peptideBondData : PeptideBondData
            enCatLigSimParam : EnCatRatesSimilarityParam
        }


    type EnCatalyticLigationSimilarModel (p : EnCatalyticLigationSimilarParamWithModel) =
        let dictionaryData =
            match DictionaryUpdateType.getEnCatLigValue() with
            | NonOptionalRateDataOnly ->
                {
                    keySetData =
                        {
                            keySet = HashSet<(EnLigCatalyst * ChiralSugar)>()
                            getReactionKey = fun (r : EnCatalyticLigationReaction) -> (r.catalyst, r.sugar)
                        }
                        |> Some
                    rateDictionary = p.enCatLigModel.rateDictionary
                }
            | AllRateData -> toDictionaryData p.enCatLigModel.rateDictionary

        let calculateSimRatesImpl rnd t (EnCatalyticLigationReaction (s, c, u)) =
            {
                reaction = s
                enCatalyst = c
                energySource = u
                getReactionData = fun r -> p.peptideBondData.findSameBondSymmetry r.peptideBond
                getMatchingReactionMult = id
                inverse = fun r -> r.peptideBond
                getCatEnantiomer = getEnantiomer
                getEnergySourceEnantiomer = getEnantiomer
                enCatReactionCreator = EnCatalyticLigationReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = p.peptideBondData.findSameBond
                getBaseRates = p.enCatLigModel.inputParams.ligationModel.getRates rnd
                getBaseCatRates = p.enCatLigModel.getRates rnd t
                enSimParams = p.enCatLigSimParam
                eeParams = p.enCatLigModel.inputParams.enCatLigationParam.enCatLigRndEeParams
                dictionaryData = dictionaryData
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateEnSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.enCatLigModel.rateDictionary
