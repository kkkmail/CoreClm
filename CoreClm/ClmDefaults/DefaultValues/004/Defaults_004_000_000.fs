namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt
open Clm.Distributions
open ClmSys.ContGenPrimitives
open Clm.ReactionRatesBase

module Defaults_004_000_000 =

    let nsd =
        [
            0.00
            0.10
            //0.20
            //0.30
            //0.40
            //0.50
            //0.60
            //0.70
            //0.80
            //0.90
            //1.00
        ]
        |> withRowNumber


    let mcl =
        [
            (0.000_000_100, 2_000.0)
            (0.000_000_200, 2_000.0)
            (0.000_000_500, 2_000.0)
            (0.000_001_000, 2_000.0)
            (0.000_002_000, 2_000.0)
            (0.000_005_000, 2_000.0)
            (0.000_010_000, 2_000.0)
            (0.000_020_000, 2_000.0)
            (0.000_050_000, 2_000.0)
        ]
        |> List.map (fun (a, b) -> Some a, b)
        |> withRowNumber


    let getDefaultValue ((n, s), (m, (a, b))) =
        let clmDefaultValueId = (4_000_000_000L + 20L * m + n) |> ClmDefaultValueId
        let description = None
        let catRateGenType = ByEnantiomerPairs FixedVal
        let successNumberType = ThresholdBased

        let defaultRateParams =
            //===========================================================
            let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam 0.1
            //===========================================================
            let synthParam = ReactionRateProviderParams.defaultSynthRndParamImpl (Some 0.001, None)
            let catSynthRndParam = (synthParam, (Some 0.000_100), 100_000.0)
            let catSynthParam = ReactionRateProviderParams.defaultCatSynthSimParam catSynthRndParam (Some s) catRateGenType
            //===========================================================
            let destrParam = ReactionRateProviderParams.defaultDestrRndParamImpl (Some 0.001, None)
            let catDestrRndParam = (destrParam, (Some 0.000_100), 100_000.0)
            let catDestrParam = ReactionRateProviderParams.defaultCatDestrSimParam catDestrRndParam (Some s) catRateGenType
            //===========================================================
            let ligParam = ReactionRateProviderParams.defaultLigRndParamImpl (1.0, 1.0)
            let catLigParam = ReactionRateProviderParams.defaultCatLigRndParam (ligParam, a, b) catRateGenType
            //===========================================================
            let rates =
                [
                    wasteRecyclingParam

                    synthParam |> SynthesisRateParam
                    catSynthParam

                    destrParam |> DestructionRateParam
                    catDestrParam

                    ligParam |> LigationRateParam
                    catLigParam
                ]
            //===========================================================

            {
                rateParams = rates
                successNumberType = successNumberType
            }

        {
            clmDefaultValueId = clmDefaultValueId
            defaultRateParams = defaultRateParams
            description = description
        }

    let defaultValues =
        (List.allPairs nsd mcl)
        |> List.map getDefaultValue
        |> updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 100, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 0.1, vary cat ligation."
