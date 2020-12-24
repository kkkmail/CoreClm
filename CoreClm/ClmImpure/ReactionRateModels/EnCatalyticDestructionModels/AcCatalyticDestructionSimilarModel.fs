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
            enCatDestrModel : AcCatalyticDestructionRandomModel
            aminoAcids : list<AminoAcid>
            enCatDestrSimParam : AcCatRatesSimilarityParam
        }


    type AcCatalyticDestructionSimilarModel (p : AcCatalyticDestructionSimilarParamWithModel) =
        let calculateSimRatesImpl rnd t (AcCatalyticDestructionReaction (s, c)) =
            let (DestructionReaction a) = s
            {
                reaction = s
                acCatalyst = c
                getReactionData = fun _ -> p.aminoAcids
                inverse = fun (DestructionReaction r) -> r.aminoAcid
                getMatchingReactionMult = fun x -> x
                getCatEnantiomer = getEnantiomer
                acCatReactionCreator = AcCatalyticDestructionReaction
                simReactionCreator = (fun e -> [ a.createSameChirality e |> DestructionReaction ])
                getCatReactEnantiomer = getEnantiomer
                getBaseRates = p.enCatDestrModel.inputParams.destructionModel.getRates rnd
                getBaseCatRates = p.enCatDestrModel.getRates rnd t
                acSimParams = p.enCatDestrSimParam
                eeParams = p.enCatDestrModel.inputParams.enCatDestrRndParam.acCatDestrRndEeParams
                rateDictionary = p.enCatDestrModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateAcSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.enCatDestrModel.rateDictionary
