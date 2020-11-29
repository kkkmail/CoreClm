namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt
open Clm.Distributions
open ClmSys.ContGenPrimitives
open Clm.ReactionRatesBase

module Defaults_003_000_218 =

    let nsd =
        [
            ( 0L, 0.00)
            ( 1L, 0.10)
            ( 2L, 0.20)
            ( 3L, 0.30)
            ( 4L, 0.40)
            ( 5L, 0.50)
            ( 6L, 0.60)
            ( 7L, 0.70)
            ( 8L, 0.80)
            ( 9L, 0.90)
            (10L, 1.00)
        ]


    let getDefaultValue (n, s) =
        let clmDefaultValueId = (3_000_218_000L + n) |> ClmDefaultValueId
        let description = None
        let catRateGenType = ByEnantiomerPairs FixedVal
        let successNumberType = ThresholdBased

        let defaultRateParams =
            //===========================================================
            let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam 5.0
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
