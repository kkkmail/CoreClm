namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.CatalyticRacemizationRandomModel

module CatalyticRacemizationSimilarModel =


    type CatalyticRacemizationSimilarParamWithModel =
        {
            catRacemSimParam : CatRatesSimilarityParam
            aminoAcids : list<AminoAcid>
            catRacemModel : CatalyticRacemizationRandomModel
        }


    type CatalyticRacemizationSimilarModel (p : CatalyticRacemizationSimilarParamWithModel) =
        let calculateSimRatesImpl rnd t (CatalyticRacemizationReaction (s, c)) =
            let (RacemizationReaction a) = s
            {
                reaction = s
                catalyst = c
                getReactionData = fun _ -> p.aminoAcids
                inverse = fun (RacemizationReaction r) -> r.aminoAcid
                getMatchingReactionMult = fun x -> x
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticRacemizationReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> [ a.createSameChirality e |> RacemizationReaction ])
                getBaseRates = p.catRacemModel.inputParams.racemizationModel.getRates rnd
                getBaseCatRates = p.catRacemModel.getRates rnd t
                simParams = p.catRacemSimParam
                eeParams = p.catRacemModel.inputParams.catRacemRndParam.catRacemRndEeParams
                rateDictionary = p.catRacemModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.catRacemModel.rateDictionary
