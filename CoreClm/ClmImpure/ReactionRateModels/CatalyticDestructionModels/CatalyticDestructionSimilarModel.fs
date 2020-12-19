namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.CatalyticDestructionRandomModel

module CatalyticDestructionSimilarModel =

    type CatalyticDestructionSimilarParamWithModel =
        {
            catDestrSimParam : CatRatesSimilarityParam
            aminoAcids : list<AminoAcid>
            catDestrModel : CatalyticDestructionRandomModel
        }


    type CatalyticDestructionSimilarModel (p : CatalyticDestructionSimilarParamWithModel) =
        let calculateSimRatesImpl rnd t (CatalyticDestructionReaction (s, c)) =
            let (DestructionReaction a) = s
            {
                reaction = s
                catalyst = c
                getReactionData = fun _ -> p.aminoAcids
                inverse = fun (DestructionReaction r) -> r.aminoAcid
                getMatchingReactionMult = fun x -> x
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticDestructionReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> [ a.createSameChirality e |> DestructionReaction ])
                getBaseRates = p.catDestrModel.inputParams.destructionModel.getRates rnd
                getBaseCatRates = p.catDestrModel.getRates rnd t
                simParams = p.catDestrSimParam
                eeParams = p.catDestrModel.inputParams.catDestrRndParam.catDestrRndEeParams
                rateDictionary = p.catDestrModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.catDestrModel.rateDictionary
