namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.EnCatalyticDestructionRandomModel

module EnCatalyticDestructionSimilarModel =

    type EnCatalyticDestructionSimilarParamWithModel =
        {
            enCatDestrModel : EnCatalyticDestructionRandomModel
            aminoAcids : list<AminoAcid>
            enCatDestrSimParam : EnCatRatesSimilarityParam
        }


    type EnCatalyticDestructionSimilarModel (p : EnCatalyticDestructionSimilarParamWithModel) =
        let dictionaryData = toDictionaryData p.enCatDestrModel.rateDictionary

        let calculateSimRatesImpl rnd t (EnCatalyticDestructionReaction (s, c, u)) =
            let (DestructionReaction a) = s
            {
                reaction = s
                enCatalyst = c
                energySource = u
                getReactionData = fun _ -> p.aminoAcids
                inverse = fun (DestructionReaction r) -> r.aminoAcid
                getMatchingReactionMult = fun x -> x
                getCatEnantiomer = getEnantiomer
                getEnergySourceEnantiomer = getEnantiomer
                enCatReactionCreator = EnCatalyticDestructionReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> [ a.createSameChirality e |> DestructionReaction ])
                getBaseRates = p.enCatDestrModel.inputParams.destructionModel.getRates rnd
                getBaseCatRates = p.enCatDestrModel.getRates t rnd
                enSimParams = p.enCatDestrSimParam
                eeParams = p.enCatDestrModel.inputParams.enCatDestrRndParam.enCatDestrRndEeParams
                dictionaryData = dictionaryData
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateEnSimRates

        member _.getRates t rnd r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.enCatDestrModel.rateDictionary
