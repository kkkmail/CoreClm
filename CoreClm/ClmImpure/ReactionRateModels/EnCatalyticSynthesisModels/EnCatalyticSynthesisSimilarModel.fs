﻿namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.EnCatalyticSynthesisRandomModel

module EnCatalyticSynthesisSimilarModel =

    type EnCatalyticSynthesisSimilarParamWithModel =
        {
            enCatSynthModel : EnCatalyticSynthesisRandomModel
            aminoAcids : list<AminoAcid>
            enCatSynthSimParam : EnCatRatesSimilarityParam
        }


    type EnCatalyticSynthesisSimilarModel (p : EnCatalyticSynthesisSimilarParamWithModel) =
        let dictionaryData = toDictionaryData p.enCatSynthModel.rateDictionary

        let calculateSimRatesImpl rnd t (EnCatalyticSynthesisReaction (s, c, u)) =
            let (SynthesisReaction a) = s
            {
                reaction = s
                enCatalyst = c
                energySource = u
                getReactionData = fun _ -> p.aminoAcids
                inverse = fun (SynthesisReaction r) -> r.aminoAcid
                getMatchingReactionMult = fun x -> x
                getCatEnantiomer = getEnantiomer
                getEnergySourceEnantiomer = getEnantiomer
                enCatReactionCreator = EnCatalyticSynthesisReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> [ a.createSameChirality e |> SynthesisReaction ])
                getBaseRates = p.enCatSynthModel.inputParams.synthesisModel.getRates rnd
                getBaseCatRates = p.enCatSynthModel.getRates t rnd
                enSimParams = p.enCatSynthSimParam
                eeParams = p.enCatSynthModel.inputParams.enCatSynthRndParam.enCatSynthRndEeParams
                dictionaryData = dictionaryData
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateEnSimRates

        member _.getRates t rnd r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.enCatSynthModel.rateDictionary
