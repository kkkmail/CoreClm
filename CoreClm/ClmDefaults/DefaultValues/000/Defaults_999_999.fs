namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt
open Clm.Distributions
open ClmSys.ContGenPrimitives
open Clm.ReactionRatesBase

module Defaults_999_999 =

    let clmDefaultValueId = 999_999L |> ClmDefaultValueId
    let description = Some "For n = 3 - 5"
    let catRateGenType = ByEnantiomerPairs DistrBased
    let successNumberType = RandomValueBased

    let defaultRateParams =
        //===========================================================
        let foodParam = ReactionRateProviderParams.defaultFoodCreationParam 0.01
        let wasteParam = ReactionRateProviderParams.defaultWasteRemovalParam 10.0
        let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam 0.1
        //===========================================================
        let synthParam = ReactionRateProviderParams.defaultSynthRndParamImpl (0.001, 0.001)
        let catSynthRndParam = (synthParam, (Some 0.02), 10_000.0)
        //let catSynthParam = ReactionRateProviderParams.defaultCatSynthRndParam catSynthRndParam catRateGenType
        let catSynthParam = ReactionRateProviderParams.defaultCatSynthSimParam catSynthRndParam (Some 0.3) catRateGenType
        //===========================================================
        let destrParam = ReactionRateProviderParams.defaultDestrRndParamImpl (0.001, 0.001)
        let catDestrRndParam = (destrParam, (Some 0.02), 100_000.0)
        //let catDestrParam = ReactionRateProviderParams.defaultCatDestrRndParam catDestrRndParam catRateGenType
        let catDestrParam = ReactionRateProviderParams.defaultCatDestrSimParam catDestrRndParam (Some 0.20) catRateGenType
        //===========================================================
        let ligParam = ReactionRateProviderParams.defaultLigRndParamImpl (1.0, 1.0)
        let catLigParam = ReactionRateProviderParams.defaultCatLigRndParam (ligParam, (Some 0.005), 2_000.0) catRateGenType
        //===========================================================
        // For n = ?
        let sedDirRndParam = (Some 0.002, 1000.0)

        // For n = 10
        //let sedDirRndParam = (Some 0.000_1, 100.0)

        // For n = 20
        //let sedDirRndParam = (Some 0.000_01, 100.0)
        //let sedDirParam = ReactionRateProviderParams.defaultSedDirRndParam sedDirRndParam
        let sedDirParam = ReactionRateProviderParams.defaultSedDirSimParam sedDirRndParam (Some 0.20)
        //===========================================================
        let sedAllParam = ReactionRateProviderParams.defaultSedAllRndParam 0.1
        //===========================================================
        let racemParam = ReactionRateProviderParams.defaultRacemRndParamImpl 0.001
        let catRacemRndParam = (racemParam, (Some 0.02), 1_000.0)
        //let catRacemParam = ReactionRateProviderParams.defaultCatRacemRndParam catRacemRndParam catRateGenType
        let catRacemParam = ReactionRateProviderParams.defaultCatRacemSimParam catRacemRndParam (Some 0.2) catRateGenType
        //===========================================================
        let rates =
            [
                foodParam
                wasteParam
                wasteRecyclingParam

                synthParam |> SynthesisRateParam
                catSynthParam

                destrParam |> DestructionRateParam
                catDestrParam

                ligParam |> LigationRateParam
                catLigParam

                sedDirParam

                //sedAllParam

                racemParam |> RacemizationRateParam
                catRacemParam
            ]
        //===========================================================

        {
            rateParams = rates
            successNumberType = successNumberType
        }


    let defaultValue =
        {
            clmDefaultValueId = clmDefaultValueId
            defaultRateParams = defaultRateParams
            description = description
        }
