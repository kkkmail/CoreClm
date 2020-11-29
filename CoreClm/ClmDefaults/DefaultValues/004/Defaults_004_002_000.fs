namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt
open Clm.Distributions
open ClmSys.ContGenPrimitives
open Clm.ReactionRatesBase

module Defaults_004_002_000 =
    let sdSim = 0.1

    /// Max 19 rows.
    let nSim =
        [
            0.0000M
            0.0010M
            0.0020M
            0.0030M
            0.0050M
            0.0070M
            0.0100M
            0.0130M
            0.0160M
            0.0200M
        ]
        |> withRowNumber


    /// Max 49 rows.
    let mScMult =
        [
            (0.0M,                 0.0M)
            (0.000_000_001M, 100_000.0M)
            (0.000_000_002M, 100_000.0M)
            (0.000_000_005M, 100_000.0M)
            (0.000_000_010M, 100_000.0M)
            (0.000_000_020M, 100_000.0M)
            (0.000_000_050M, 100_000.0M)
            (0.000_000_100M, 100_000.0M)
        ]
        |> withRowNumber


    let getDefaultValue ((n, similarity), (m, (scarcity, multiplier))) =
        let clmDefaultValueId = (4_002_000_000L + 20L * m + n) |> ClmDefaultValueId
        printfn "clmDefaultValueId = %A, similarity = %A, scarcity = %A, multiplier = %A" clmDefaultValueId similarity scarcity multiplier

        let description = None
        let catRateGenType = ByEnantiomerPairs FixedVal
        let successNumberType = ThresholdBased

        let defaultRateParams =
            //===========================================================
            let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam 0.1
            //===========================================================
            let synthParam = ReactionRateProviderParams.defaultSynthRndParamImpl (Some 0.001, None)
            let catSynthRndParam = (synthParam, (Some 0.000_100), 100_000.0)

            let catSynthParam =
                ReactionRateProviderParams.defaultCatSynthSimParam catSynthRndParam (Some sdSim) catRateGenType
            //===========================================================
            let destrParam = ReactionRateProviderParams.defaultDestrRndParamImpl (Some 0.001, None)

            let catDestrRndParam = (destrParam, (Some 0.000_100), 100_000.0)

//            // For n = 2 - 3. Remove once no longer needed.
//            let catDestrRndParam = (destrParam, (Some 0.30), 100_000.0)

            let catDestrParam =
                ReactionRateProviderParams.defaultCatDestrSimParam catDestrRndParam (Some sdSim) catRateGenType
            //===========================================================
            let ligParam = ReactionRateProviderParams.defaultLigRndParamImpl (0.001, 0.001)

            let catLigParam =
                ReactionRateProviderParams.defaultCatLigSimParam (ligParam, Some (double scarcity), (double multiplier)) (Some (double similarity)) catRateGenType
            //===========================================================
            let rates =
                [
                    wasteRecyclingParam

                    synthParam |> SynthesisRateParam
                    catSynthParam

                    destrParam |> DestructionRateParam
                    catDestrParam

                    ligParam |> LigationRateParam
                    if (scarcity > 0.0M) then catLigParam
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
        (List.allPairs nSim mScMult)
        |> List.filter (fun ((_, similarity), (_, (scarcity, _))) -> (scarcity = 0.0M && similarity = 0.0M) || scarcity > 0.0M)
        |> List.map getDefaultValue
        |> updateDescription "Cat lig with similarity playground."
