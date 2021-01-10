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
                getBaseRates = p.acCatDestrModel.inputParams.destructionModel.getRates rnd
                getBaseCatRates = p.acCatDestrModel.getRates rnd t
                acSimParams = p.acCatDestrSimParam
                acEeParams = p.acCatDestrModel.inputParams.acCatDestrRndParam.acCatDestrRndEeParams
                rateDictionary = p.acCatDestrModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
                dictionaryUpdateType = AllRateData
            }
            |> calculateAcSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.acCatDestrModel.rateDictionary
