namespace ClmImpure.ReactionRateModels

open System.Collections.Generic
open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.AcFwdCatalyticLigationRandomModel
open ClmSys.ModelData

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

//        do printfn $"AcFwdCatalyticLigationSimilarModel: dictionaryData = {dictionaryData}."

        let calculateSimRatesImpl rnd t (AcFwdCatalyticLigationReaction (s, c)) =
            let info =
                {
                    reaction = s
                    acCatalyst = c
                    acSimParams = p.acFwdCatLigSimParam
                    acEeParams = p.acFwdCatLigModel.inputParams.acFwdCatLigationParam.acFwdCatLigRndEeParams
                    dictionaryData = dictionaryData
                    acRateDictionary = p.acFwdCatLigModel.inputParams.activationModel.dictionaryData.rateDictionary

                    proxy =
                        {
                            acCatRatesInfoProxy =
                                {
                                    getNonActivated = fun e -> e.peptide
                                    getCatEnantiomer = getEnantiomer
                                    acCatReactionCreator = AcFwdCatalyticLigationReaction
                                    getBaseRates = p.acFwdCatLigModel.inputParams.ligationModel.getRates rnd
                                    createActivationData = p.acFwdCatLigModel.inputParams.activationModel.createActivationData rnd
                                    getAcEnantiomer = getEnantiomer
                                    rateGenerationType = t
                                    rnd = rnd
                                }

                            inverse = fun r -> r.peptideBond
                            getReactionData = fun r -> p.peptideBondData.findSameBondSymmetry r.peptideBond
                            simReactionCreator = p.peptideBondData.findSameBond
                            getCatReactEnantiomer = getEnantiomer
                            getBaseCatRates = p.acFwdCatLigModel.getRates t
                            getMatchingReactionMult = id
                            tryGetBaseCatRates = p.acFwdCatLigModel.tryGetRates
                        }
                }

            info
            |> calculateAcSimRates

        member _.getRates t rnd r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.acFwdCatLigModel.rateDictionary
