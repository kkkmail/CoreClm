namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.AcCatalyticRacemizationRandomModel

module AcCatalyticRacemizationSimilarModel =

    type AcCatalyticRacemizationSimilarParamWithModel =
        {
            acCatRacemModel : AcCatalyticRacemizationRandomModel
            aminoAcids : list<AminoAcid>
            acCatRacemSimParam : AcCatRatesSimilarityParam
        }


    type AcCatalyticRacemizationSimilarModel (p : AcCatalyticRacemizationSimilarParamWithModel) =
        let dictionaryData = toDictionaryData p.acCatRacemModel.rateDictionary

        let calculateSimRatesImpl rnd t (AcCatalyticRacemizationReaction (s, c)) =
            let (RacemizationReaction a) = s

            let info =
                {
                    reaction = s
                    acCatalyst = c
                    acSimParams = p.acCatRacemSimParam
                    acEeParams = p.acCatRacemModel.inputParams.acCatRacemRndParam.acCatRacemRndEeParams
                    dictionaryData = dictionaryData
                    acRateDictionary = p.acCatRacemModel.inputParams.activationModel.dictionaryData.rateDictionary

                    proxy =
                        {
                            acCatRatesInfoProxy =
                                {
                                    getNonActivated = fun e -> e.peptide
                                    getCatEnantiomer = getEnantiomer
                                    acCatReactionCreator = AcCatalyticRacemizationReaction
                                    getBaseRates = p.acCatRacemModel.inputParams.racemizationModel.getRates rnd
                                    createActivationData = p.acCatRacemModel.inputParams.activationModel.createActivationData rnd
                                    getAcEnantiomer = getEnantiomer
                                    rateGenerationType = t
                                    rnd = rnd
                                }

                            inverse = fun (RacemizationReaction r) -> r.aminoAcid
                            getReactionData = fun _ -> p.aminoAcids
                            simReactionCreator = (fun e -> [ a.createSameChirality e |> RacemizationReaction ])
                            getCatReactEnantiomer = getEnantiomer
                            getBaseCatRates = p.acCatRacemModel.getRates t
                            getMatchingReactionMult = id
                            tryGetBaseCatRates = p.acCatRacemModel.tryGetRates
                        }
                }

            info
            |> calculateAcSimRates

        member _.getRates t rnd r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.acCatRacemModel.rateDictionary
