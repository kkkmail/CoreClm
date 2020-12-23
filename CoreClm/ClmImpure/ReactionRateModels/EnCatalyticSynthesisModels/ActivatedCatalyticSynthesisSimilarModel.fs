namespace ClmImpure.ReactionRateModels

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
                getBaseCatRates = p.enCatSynthModel.getRates rnd t
                enSimParams = p.enCatSynthSimParam
                eeParams = p.enCatSynthModel.inputParams.enCatSynthRndParam.enCatSynthRndEeParams
                rateDictionary = p.enCatSynthModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateEnSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.enCatSynthModel.rateDictionary
