namespace ClmImpure.ReactionRateModels

open System.Collections.Generic
open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.AcBkwCatalyticLigationRandomModel
open ClmSys.ModelData

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
            let info =
                {
                    reaction = s
                    acCatalyst = c
                    acSimParams = p.acBkwCatLigSimParam
                    acEeParams = p.acBkwCatLigModel.inputParams.acBkwCatLigationParam.acBkwCatLigRndEeParams
                    dictionaryData = dictionaryData
                    acRateDictionary = p.acBkwCatLigModel.inputParams.activationModel.dictionaryData.rateDictionary

                    proxy =
                        {
                            acCatRatesInfoProxy =
                                {
                                    getNonActivated = fun e -> e.peptide
                                    getCatEnantiomer = getEnantiomer
                                    acCatReactionCreator = AcBkwCatalyticLigationReaction
                                    getBaseRates = p.acBkwCatLigModel.inputParams.ligationModel.getRates rnd
                                    createActivationData = p.acBkwCatLigModel.inputParams.activationModel.createActivationData rnd
                                    getAcEnantiomer = getEnantiomer
                                    rateGenerationType = t
                                    rnd = rnd
                                }

                            inverse = fun r -> r.peptideBond
                            getReactionData = fun r -> p.peptideBondData.findSameBondSymmetry r.peptideBond
                            simReactionCreator = p.peptideBondData.findSameBond
                            getCatReactEnantiomer = getEnantiomer
                            getBaseCatRates = p.acBkwCatLigModel.getRates t
                            getMatchingReactionMult = id
                            tryGetBaseCatRates = p.acBkwCatLigModel.tryGetRates
                        }
                }

            info
            |> calculateAcSimRates

        member _.getRates t rnd r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.acBkwCatLigModel.rateDictionary
