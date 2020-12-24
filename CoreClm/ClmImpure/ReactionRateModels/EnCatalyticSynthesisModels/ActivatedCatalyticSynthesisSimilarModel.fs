namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ActivatedCatalyticSynthesisRandomModel

module ActivatedCatalyticSynthesisSimilarModel =

    type ActivatedCatalyticSynthesisSimilarParamWithModel =
        {
            acCatSynthModel : ActivatedCatalyticSynthesisRandomModel
            aminoAcids : list<AminoAcid>
            acCatSynthSimParam : ActivatedCatRatesSimilarityParam
        }


    type ActivatedCatalyticSynthesisSimilarModel (p : ActivatedCatalyticSynthesisSimilarParamWithModel) =
        let calculateSimRatesImpl rnd t (ActivatedCatalyticSynthesisReaction (s, c)) =
            let (SynthesisReaction a) = s
            {
                reaction = s
                acCatalyst = c
                getReactionData = fun _ -> p.aminoAcids
                inverse = fun (SynthesisReaction r) -> r.aminoAcid
                getMatchingReactionMult = fun x -> x
                getCatEnantiomer = getEnantiomer
                acCatReactionCreator = ActivatedCatalyticSynthesisReaction
                simReactionCreator = (fun e -> [ a.createSameChirality e |> SynthesisReaction ])
                getCatReactEnantiomer = getEnantiomer
                getBaseRates = p.acCatSynthModel.inputParams.synthesisModel.getRates rnd
                getBaseCatRates = p.acCatSynthModel.getRates rnd t
                acSimParams = p.acCatSynthSimParam
                eeParams = p.acCatSynthModel.inputParams.acCatSynthRndParam.acCatSynthRndEeParams
                rateDictionary = p.acCatSynthModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateActivatedSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.acCatSynthModel.rateDictionary
