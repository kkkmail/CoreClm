namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.AcCatalyticRacemizationRandomModel

module AcCatalyticRacemizationSimilarModel =

    type AcCatalyticRacemizationSimilarParamWithModel =
        {
            acCatRacemModel : AcCatalyticRacemizationRandomModel
            aminoAcids : list<AminoAcid>
            acCatRacemSimParam : AcCatRatesSimilarityParam
        }


    type AcCatalyticRacemizationSimilarModel (p : AcCatalyticRacemizationSimilarParamWithModel) =
        let calculateSimRatesImpl rnd t (AcCatalyticRacemizationReaction (s, c)) =
            let (RacemizationReaction a) = s
            {
                reaction = s
                acCatalyst = c
                getReactionData = fun _ -> p.aminoAcids
                inverse = fun (RacemizationReaction r) -> r.aminoAcid
                getMatchingReactionMult = fun x -> x
                getCatEnantiomer = getEnantiomer
                acCatReactionCreator = AcCatalyticRacemizationReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> [ a.createSameChirality e |> RacemizationReaction ])
                getBaseRates = p.acCatRacemModel.inputParams.racemizationModel.getRates rnd
                getBaseCatRates = p.acCatRacemModel.getRates rnd t
                acSimParams = p.acCatRacemSimParam
                acEeParams = p.acCatRacemModel.inputParams.acCatRacemRndParam.acCatRacemRndEeParams
                rateDictionary = p.acCatRacemModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
                dictionaryUpdateType = AllRateData
            }
            |> calculateAcSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.acCatRacemModel.rateDictionary
