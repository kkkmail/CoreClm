namespace ClmImpure.ReactionRateModels

open System.Collections.Generic
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
            dictionaryUpdateType : DictionaryUpdateType
        }


    type AcFwdCatalyticLigationSimilarModel (p : AcFwdCatalyticLigationSimilarParamWithModel) =
        let dictionaryData =
            match p.dictionaryUpdateType with
            | AllRateData -> toDictionaryData p.acFwdCatLigModel.rateDictionary
            | NonOptionalRateDataOnly ->
                {
                    keySetData =
                        {
                            keySet = HashSet<AcFwdLigCatalyst>()
                            getReactionKey = fun (r : AcFwdCatalyticLigationReaction) -> r.catalyst
                        }
                        |> Some
                    rateDictionary = p.acFwdCatLigModel.rateDictionary
                }

        do printfn $"AcFwdCatalyticLigationSimilarModel: dictionaryData = {dictionaryData}."

        let calculateSimRatesImpl rnd t (AcFwdCatalyticLigationReaction (s, c)) =
            {
                reaction = s
                acCatalyst = c
                getReactionData = fun r -> p.peptideBondData.findSameBondSymmetry r.peptideBond
                getMatchingReactionMult = id
                inverse = fun r -> r.peptideBond
                getCatEnantiomer = getEnantiomer
                acCatReactionCreator = AcFwdCatalyticLigationReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = p.peptideBondData.findSameBond
                getBaseRates = p.acFwdCatLigModel.inputParams.ligationModel.getRates rnd
                getBaseCatRates = p.acFwdCatLigModel.getRates t
                tryGetBaseCatRates = p.acFwdCatLigModel.tryGetRates
                acSimParams = p.acFwdCatLigSimParam
                acEeParams = p.acFwdCatLigModel.inputParams.acFwdCatLigationParam.acFwdCatLigRndEeParams
                dictionaryData = dictionaryData
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateAcSimRates

        member _.getRates t rnd r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.acFwdCatLigModel.rateDictionary
