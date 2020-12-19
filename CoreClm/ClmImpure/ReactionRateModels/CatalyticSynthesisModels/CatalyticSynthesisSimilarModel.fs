namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.CatalyticSynthesisRandomModel

module CatalyticSynthesisSimilarModel =

    type CatalyticSynthesisSimilarParamWithModel =
        {
            catSynthModel : CatalyticSynthesisRandomModel
            aminoAcids : list<AminoAcid>
            catSynthSimParam : CatRatesSimilarityParam
        }


    type CatalyticSynthesisSimilarModel (p : CatalyticSynthesisSimilarParamWithModel) =
        let calculateSimRatesImpl rnd t (CatalyticSynthesisReaction (s, c)) =
            let (SynthesisReaction a) = s
            {
                reaction = s
                catalyst = c
                getReactionData = fun _ -> p.aminoAcids
                inverse = fun (SynthesisReaction r) -> r.aminoAcid
                getMatchingReactionMult = fun x -> x
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticSynthesisReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> [ a.createSameChirality e |> SynthesisReaction ])
                getBaseRates = p.catSynthModel.inputParams.synthesisModel.getRates rnd
                getBaseCatRates = p.catSynthModel.getRates rnd t
                simParams = p.catSynthSimParam
                eeParams = p.catSynthModel.inputParams.catSynthRndParam.catSynthRndEeParams
                rateDictionary = p.catSynthModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.catSynthModel.rateDictionary
