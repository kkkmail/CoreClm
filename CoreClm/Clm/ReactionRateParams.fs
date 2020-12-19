namespace Clm

open System.Collections.Generic
open FSharp.Collections
open Clm.Distributions
open Clm.ReactionTypes
open Clm.Substances
open Clm.ReactionRatesBase

module ReactionRateParams =
    type FoodCreationParam =
        {
            foodCreationRate : double
        }


    type WasteRemovalParam =
        {
            wasteRemovalRate : double
        }


    type WasteRecyclingParam =
        {
            wasteRecyclingRate : double
        }


    type SynthesisRandomParam =
        {
            synthesisDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }


    type SynthesisParam =
        | SynthRndParam of SynthesisRandomParam


    type SugarSynthesisRandomParam =
        {
            sugarSynthesisDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }


    type SugarSynthesisParam =
        | SugarSynthRndParam of SugarSynthesisRandomParam


    type CatalyticSynthesisRandomParam =
        {
            synthesisParam : SynthesisParam
            catSynthRndEeParams : CatRatesEeParam
        }


    type CatalyticSynthesisSimilarParam =
        {
            catSynthParam : CatalyticSynthesisRandomParam
            catSynthSimParam : CatRatesSimilarityParam
        }


    type CatalyticSynthesisParam =
        | CatSynthRndParam of CatalyticSynthesisRandomParam
        | CatSynthSimParam of CatalyticSynthesisSimilarParam


    type EnCatalyticSynthesisRandomParam =
        {
            synthesisParam : SynthesisParam
            enCatSynthRndEeParams : EnCatRatesEeParam
        }


    type EnCatalyticSynthesisSimilarParam =
        {
            enCatSynthParam : EnCatalyticSynthesisRandomParam
            enCatSynthSimParam : EnCatRatesSimilarityParam
        }


    type EnCatalyticSynthesisParam =
        | EnCatSynthRndParam of EnCatalyticSynthesisRandomParam
        | EnCatSynthSimParam of EnCatalyticSynthesisSimilarParam


    type DestructionRandomParam =
        {
            destructionDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }


    type DestructionParam =
        | DestrRndParam of DestructionRandomParam


    type CatalyticDestructionRandomParam =
        {
            catDestrRndEeParams : CatRatesEeParam
            destructionParam : DestructionParam
        }


    type CatalyticDestructionSimilarParam =
        {
            catDestrSimParam : CatRatesSimilarityParam
            catDestrParam : CatalyticDestructionRandomParam
        }


    type CatalyticDestructionParam =
        | CatDestrRndParam of CatalyticDestructionRandomParam
        | CatDestrSimParam of CatalyticDestructionSimilarParam


    type EnCatalyticDestructionRandomParam =
        {
            destructionParam : DestructionParam
            enCatDestrRndEeParams : EnCatRatesEeParam
        }


    type EnCatalyticDestructionSimilarParam =
        {
            enCatDestrParam : EnCatalyticDestructionRandomParam
            enCatDestrSimParam : EnCatRatesSimilarityParam
        }


    type EnCatalyticDestructionParam =
        | EnCatDestrRndParam of EnCatalyticDestructionRandomParam
        | EnCatDestrSimParam of EnCatalyticDestructionSimilarParam


    type SedDirRatesEeParam =
        {
            sedDirRateMultiplierDistr : RateMultiplierDistribution
            eeDistribution : EeDistribution option
        }

        static member defaultValue =
            {
                sedDirRateMultiplierDistr = NoneRateMult
                eeDistribution = None
            }


    type SedimentationDirectRandomParam =
        {
            sedDirRatesEeParam : SedDirRatesEeParam
            sedDirDistribution : Distribution
            forwardScale : double option
        }


    type SedDirRatesInfo =
        {
            sedFormingSubst : SedDirReagent
            sedDirAgent : SedDirAgent
            getBaseRates : SedimentationDirectReaction -> RateData
            eeParams : SedDirRatesEeParam
            rateGenerationType : RateGenerationType
            rnd : RandomValueGetter
        }


    type SedDirSimilarityParam =
        {
            sedDirSimBaseDistribution : Distribution
            getRateMultiplierDistr : RateMultiplierDistributionGetter
            getEeDistr : EeDistributionGetter
        }


    type SedDirRatesSimInfo =
        {
            sedDirRatesInfo : SedDirRatesInfo

            aminoAcids : list<AminoAcid>
            reagents : Map<AminoAcid, list<SedDirReagent>>
            simParams : SedDirSimilarityParam
            rateDictionary : Dictionary<SedimentationDirectReaction, RateData>
        }


    type SedimentationDirectSimilarParam =
        {
            sedDirParam : SedimentationDirectRandomParam
            sedDirSimParam : SedDirSimilarityParam
        }


    type SedimentationDirectParam =
        | SedDirRndParam of SedimentationDirectRandomParam
        | SedDirSimParam of SedimentationDirectSimilarParam


    type SedimentationAllRandomParam =
        {
            sedimentationAllDistribution : Distribution
            forwardScale : double option
        }


    type SedimentationAllParam =
        | SedAllRndParam of SedimentationAllRandomParam


    type LigationRandomParam =
        {
            ligationDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }


    type LigationParam =
        | LigRndParam of LigationRandomParam


    type CatalyticLigationRandomParam =
        {
            ligationParam : LigationParam
            catLigRndEeParams : CatRatesEeParam
        }


    type CatalyticLigationSimilarParam =
        {
            catLigSimParam : CatRatesSimilarityParam
            catLigParam : CatalyticLigationRandomParam
        }


    type CatalyticLigationParam =
        | CatLigRndParam of CatalyticLigationRandomParam
        | CatLigSimParam of CatalyticLigationSimilarParam


    type EnCatalyticLigationRandomParam =
        {
            ligationParam : LigationParam
            enCatLigRndEeParams : EnCatRatesEeParam
        }


    type EnCatalyticLigationSimilarParam =
        {
            enCatLigSimParam : EnCatRatesSimilarityParam
            enCatLigParam : EnCatalyticLigationRandomParam
        }


    type EnCatalyticLigationParam =
        | EnCatLigRndParam of EnCatalyticLigationRandomParam
        | EnCatLigSimParam of EnCatalyticLigationSimilarParam


    type RacemizationRandomParam =
        {
            racemizationDistribution : Distribution
            forwardScale : double option
        }


    type RacemizationParam =
        | RacemRndParam of RacemizationRandomParam


    type CatalyticRacemizationRandomParam =
        {
            racemizationParam : RacemizationParam
            catRacemRndEeParams : CatRatesEeParam
        }


    type CatalyticRacemizationSimilarParam =
        {
            catRacemParam : CatalyticRacemizationRandomParam
            catRacemSimParam : CatRatesSimilarityParam
        }


    type CatalyticRacemizationParam =
        | CatRacemRndParam of CatalyticRacemizationRandomParam
        | CatRacemSimParam of CatalyticRacemizationSimilarParam


    type EnCatalyticRacemizationRandomParam =
        {
            racemizationParam : RacemizationParam
            enCatRacemRndEeParams : EnCatRatesEeParam
        }


    type EnCatalyticRacemizationSimilarParam =
        {
            enCatRacemParam : EnCatalyticRacemizationRandomParam
            enCatRacemSimParam : EnCatRatesSimilarityParam
        }


    type EnCatalyticRacemizationParam =
        | EnCatRacemRndParam of EnCatalyticRacemizationRandomParam
        | EnCatRacemSimParam of EnCatalyticRacemizationSimilarParam
