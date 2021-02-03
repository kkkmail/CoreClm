namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.AcCatalyticDestructionRandomModel

module AcCatalyticDestructionSimilarModel =

    type AcCatalyticDestructionSimilarParamWithModel =
        {
            acCatDestrModel : AcCatalyticDestructionRandomModel
            aminoAcids : list<AminoAcid>
            acCatDestrSimParam : AcCatRatesSimilarityParam
        }


    type AcCatalyticDestructionSimilarModel (p : AcCatalyticDestructionSimilarParamWithModel) =
        let dictionaryData = toDictionaryData p.acCatDestrModel.rateDictionary

        let calculateSimRatesImpl rnd t (AcCatalyticDestructionReaction (s, c)) =
            let (DestructionReaction a) = s

            let info =
                {
                    reaction = s
                    acCatalyst = c
                    acSimParams = p.acCatDestrSimParam
                    acEeParams = p.acCatDestrModel.inputParams.acCatDestrRndParam.acCatDestrRndEeParams
                    dictionaryData = dictionaryData
                    acRateDictionary = p.acCatDestrModel.inputParams.activationModel.dictionaryData.rateDictionary

                    proxy =
                        {
                            acCatRatesInfoProxy =
                                {
                                    getNonActivated = fun e -> e.peptide
                                    getCatEnantiomer = getEnantiomer
                                    acCatReactionCreator = AcCatalyticDestructionReaction
                                    getBaseRates = p.acCatDestrModel.inputParams.destructionModel.getRates rnd
                                    createActivationData = p.acCatDestrModel.inputParams.activationModel.createActivationData rnd
                                    getAcEnantiomer = getEnantiomer
                                    rateGenerationType = t
                                    rnd = rnd
                                }

                            inverse = fun (DestructionReaction r) -> r.aminoAcid
                            getReactionData = fun _ -> p.aminoAcids
                            simReactionCreator = (fun e -> [ a.createSameChirality e |> DestructionReaction ])
                            getCatReactEnantiomer = getEnantiomer
                            getBaseCatRates = p.acCatDestrModel.getRates t
                            getMatchingReactionMult = id
                            tryGetBaseCatRates = p.acCatDestrModel.tryGetRates
                        }
                }

            info
            |> calculateAcSimRates

        member _.getRates t rnd r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.acCatDestrModel.rateDictionary
