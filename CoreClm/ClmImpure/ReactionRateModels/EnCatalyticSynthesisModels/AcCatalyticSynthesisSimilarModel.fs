namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.AcCatalyticSynthesisRandomModel

module AcCatalyticSynthesisSimilarModel =

    type AcCatalyticSynthesisSimilarParamWithModel =
        {
            acCatSynthModel : AcCatalyticSynthesisRandomModel
            aminoAcids : list<AminoAcid>
            acCatSynthSimParam : AcCatRatesSimilarityParam
        }


    type AcCatalyticSynthesisSimilarModel (p : AcCatalyticSynthesisSimilarParamWithModel) =
        let dictionaryData = toDictionaryData p.acCatSynthModel.rateDictionary

        let calculateSimRatesImpl rnd (t : RateGenerationType) (AcCatalyticSynthesisReaction (s, c)) =
            let (SynthesisReaction a) = s

            let info =
                {
                    reaction = s
                    acCatalyst = c
                    acSimParams = p.acCatSynthSimParam
                    acEeParams = p.acCatSynthModel.inputParams.acCatSynthRndParam.acCatSynthRndEeParams
                    dictionaryData = dictionaryData
                    acRateDictionary = p.acCatSynthModel.inputParams.activationModel.dictionaryData.rateDictionary

                    proxy =
                        {
                            acCatRatesInfoProxy =
                                {
                                    getNonActivated = fun e -> e.peptide
                                    getCatEnantiomer = getEnantiomer
                                    acCatReactionCreator = AcCatalyticSynthesisReaction
                                    getBaseRates = p.acCatSynthModel.inputParams.synthesisModel.getRates rnd
                                    createActivationData = p.acCatSynthModel.inputParams.activationModel.createActivationData rnd
                                    getAcEnantiomer = getEnantiomer
                                    rateGenerationType = t
                                    rnd = rnd
                                }

                            inverse = fun (SynthesisReaction r) -> r.aminoAcid
                            getReactionData = fun _ -> p.aminoAcids
                            simReactionCreator = (fun e -> [ a.createSameChirality e |> SynthesisReaction ])
                            getCatReactEnantiomer = getEnantiomer
                            getBaseCatRates = p.acCatSynthModel.getRates t
                            getMatchingReactionMult = id
                            tryGetBaseCatRates = p.acCatSynthModel.tryGetRates
                        }
                }

            info
            |> calculateAcSimRates

        member _.getRates t rnd r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.acCatSynthModel.rateDictionary
