namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt
open Clm.Distributions
open ClmSys.ContGenPrimitives
open Clm.ReactionRatesBase

module Defaults_001_001_022 =

    let ns =
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


    /// Same as 1_000_022_XYZ but without destrParam.
    let getDefaultValue (n, s) =
        let clmDefaultValueId = (1_001_022_000L + n) |> ClmDefaultValueId
        let description = None
        let catRateGenType = ByEnantiomerPairs FixedVal
        let successNumberType = ThresholdBased

        let defaultRateParams =
            //===========================================================
            let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam 0.1
            //===========================================================
            let synthParam = ReactionRateProviderParams.defaultSynthRndParamImpl (Some 0.001, None)
            let catSynthRndParam = (synthParam, (Some 0.000_050), 100_000.0)
            let catSynthParam = ReactionRateProviderParams.defaultCatSynthSimParam catSynthRndParam (Some s) catRateGenType
            //===========================================================
            let ligParam = ReactionRateProviderParams.defaultLigRndParamImpl (1.0, 1.0)
            //===========================================================
            let rates =
                [
                    wasteRecyclingParam

                    synthParam |> SynthesisRateParam
                    catSynthParam

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
        printfn "\n"

        ns
        |> List.map getDefaultValue
        |> updateDescription "Catalytic synthesis / forward only for n = 20 (catSynthScarcity = 50, vary catSynthSim) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, no destruction."
        