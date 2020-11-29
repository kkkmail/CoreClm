namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.EnCatalyticRacemizationRandomModel

module EnCatalyticRacemizationSimilarModel =

    type EnCatalyticRacemizationSimilarParamWithModel =
        {
            enCatRacemModel : EnCatalyticRacemizationRandomModel
            aminoAcids : list<AminoAcid>
            enCatRacemSimParam : EnCatRatesSimilarityParam
        }


    type EnCatalyticRacemizationSimilarModel (p : EnCatalyticRacemizationSimilarParamWithModel) =
        let calculateSimRatesImpl rnd t (EnCatalyticRacemizationReaction (s, c, u)) =
            let (RacemizationReaction a) = s
            {
                reaction = s
                enCatalyst = c
                energySource = u
                getReactionData = fun _ -> p.aminoAcids
                inverse = fun (RacemizationReaction r) -> r.aminoAcid
                getMatchingReactionMult = fun x -> x
                getCatEnantiomer = getEnantiomer
                getEnergySourceEnantiomer = getEnantiomer
                enCatReactionCreator = EnCatalyticRacemizationReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> [ a.createSameChirality e |> RacemizationReaction ])
                getBaseRates = p.enCatRacemModel.inputParams.racemizationModel.getRates rnd
                getBaseCatRates = p.enCatRacemModel.getRates rnd t
                enSimParams = p.enCatRacemSimParam
                eeParams = p.enCatRacemModel.inputParams.enCatRacemRndParam.enCatRacemRndEeParams
                rateDictionary = p.enCatRacemModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateEnSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.enCatRacemModel.rateDictionary
