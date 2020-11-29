namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt
open Clm.Distributions
open ClmSys.ContGenPrimitives
open Clm.ReactionRatesBase

module Defaults_002_000_318 =

    let nsd =
        [
            0.00
            0.10
            0.20
            0.30
            0.40
            0.50
            0.60
            0.70
            0.80
            0.90
            1.00
        ]
        |> withRowNumber


    let mw =
        [
            0.001000
            0.002000
            0.005000
            0.010000
            0.020000
            0.050000
            0.000000
            0.000001
            0.000002
            0.000005
            0.000010
            0.000020
            0.000050
            0.000100
            0.000200
            0.000500
        ]
        |> withRowNumber


    let getDefaultValue ((n, s), (m, w)) =
        let clmDefaultValueId = (2_000_318_000L + 20L * m + n) |> ClmDefaultValueId
        let description = None
        let catRateGenType = ByEnantiomerPairs FixedVal
        let successNumberType = ThresholdBased

        let defaultRateParams =
            //===========================================================
            let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam w
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

    let defaultValues = (List.allPairs nsd mw) |> List.map getDefaultValue
