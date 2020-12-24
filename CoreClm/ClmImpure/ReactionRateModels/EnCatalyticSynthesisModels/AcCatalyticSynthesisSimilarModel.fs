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
        let calculateSimRatesImpl rnd t (AcCatalyticSynthesisReaction (s, c)) =
            let (SynthesisReaction a) = s
            {
                reaction = s
                acCatalyst = c
                getReactionData = fun _ -> p.aminoAcids
                inverse = fun (SynthesisReaction r) -> r.aminoAcid
                getMatchingReactionMult = fun x -> x
                getCatEnantiomer = getEnantiomer
                acCatReactionCreator = AcCatalyticSynthesisReaction
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
            |> calculateAcSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.acCatSynthModel.rateDictionary
