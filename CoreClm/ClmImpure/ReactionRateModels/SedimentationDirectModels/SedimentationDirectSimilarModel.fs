﻿namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase
open ClmImpure.ReactionRateModels.SedimentationDirectRandomModel

module SedimentationDirectSimilarModel =

    type SedimentationDirectSimilarParamWithModel =
        {
            sedDirSimParam : SedDirSimilarityParam
            aminoAcids : list<AminoAcid>
            reagents : Map<AminoAcid, list<SedDirReagent>>
            sedDirModel : SedimentationDirectRandomModel
        }


    type SedimentationDirectSimilarModel (p : SedimentationDirectSimilarParamWithModel) =
        let dictionaryData = toDictionaryData p.sedDirModel.rateDictionary

        let calculateSimRatesImpl rnd t (SedimentationDirectReaction (s, c)) =
            {
                sedDirRatesInfo =
                    {
                        sedFormingSubst = s
                        sedDirAgent = c
                        getBaseRates = p.sedDirModel.getRates t rnd
                        eeParams = p.sedDirModel.inputParams.sedDirRatesEeParam
                        rateGenerationType = t
                        rnd = rnd
                    }

                aminoAcids = p.aminoAcids
                reagents = p.reagents
                simParams = p.sedDirSimParam
                dictionaryData = dictionaryData
            }
            |> calculateSedDirSimRates

        member _.getRates t rnd r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.sedDirModel.rateDictionary
