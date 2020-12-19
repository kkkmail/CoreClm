namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt
open Clm.Distributions
open ClmSys.ContGenPrimitives
open Clm.ReactionRatesBase

module Defaults_004_001_000 =

    let nsd =
        [
            0.00
            0.10
            0.20
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
            (0.000_050, 100_000.0)
            (0.000_100, 100_000.0)
            (0.000_200, 100_000.0)
        ]
        |> List.map (fun (a, b) -> Some a, b)
        |> withRowNumber


    let getDefaultValue ((n, s), (m, (a, b))) =
        let clmDefaultValueId = (4_001_000_000L + 20L * m + n) |> ClmDefaultValueId
        let description = None
        let catRateGenType = ByEnantiomerPairs FixedVal
        let successNumberType = ThresholdBased

        let defaultRateParams =
            //===========================================================
            let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam 0.1
            //===========================================================
            let synthParam = ReactionRateProviderParams.defaultSynthRndParamImpl (Some 0.001, None)
            let catSynthRndParam = (synthParam, a, b)
            let catSynthParam = ReactionRateProviderParams.defaultCatSynthSimParam catSynthRndParam (Some s) catRateGenType
            //===========================================================
            let destrParam = ReactionRateProviderParams.defaultDestrRndParamImpl (Some 0.001, None)
            let catDestrRndParam = (destrParam, a, b)
            let catDestrParam = ReactionRateProviderParams.defaultCatDestrSimParam catDestrRndParam (Some s) catRateGenType
            //===========================================================
            let ligParam = ReactionRateProviderParams.defaultLigRndParamImpl (1.0, 1.0)
            //===========================================================
            let rates =
                [
                    wasteRecyclingParam

                    synthParam |> SynthesisRateParam
                    catSynthParam

                    destrParam |> DestructionRateParam
                    catDestrParam

                    ligParam |> LigationRateParam
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
        |> updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 5, m = 5 (vary both scarcity param and both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 0.1."
