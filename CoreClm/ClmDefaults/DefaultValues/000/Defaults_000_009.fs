namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt
open Clm.Distributions
open ClmSys.ContGenPrimitives
open Clm.ReactionRatesBase

module Defaults_000_009 =

    let nsd =
        [
            ( 0L,  10,  10) // used
            ( 1L,  10,   5) // used
            ( 2L,  10,  20) // used
            ( 3L,   5,  10) // used
            ( 4L,   5,   5) // used
            ( 5L,   5,  20) // used
            ( 6L,  20,  10) // used
            ( 7L,  20,   5) // used
            ( 8L,  20,  20) // used
            ( 9L,  10,   2)
            (10L,  10,  50) // used
            (11L,  10, 100)
            (12L,   5,   2)
            (13L,   5,  50) // used
            (14L,   5, 100)
            (15L,  20,   2)
            (16L,  20,  50) // used
            (17L,  20, 100)
            (18L,   2,   2) // used
            (19L,   2,   5)
            (20L,   2,  10)
            (21L,   2,  20)
            (22L,   2,  50)
            (23L,   2, 100)
            (24L,  50,   2)
            (25L,  50,   5) // used
            (26L,  50,  10) // used
            (27L,  50,  20) // used
            (28L,  50,  50) // used
            (29L,  50, 100)
            (30L, 100,   2)
            (31L, 100,   5)
            (32L, 100,  10)
            (33L, 100,  20)
            (34L, 100,  50)
            (35L, 100, 100) // used

            (36L, 200,   2)
            (37L, 200,   5)
            (38L, 200,  10)
            (39L, 200,  20)
            (40L, 200,  50)
            (41L, 200, 100)
            (42L, 200, 200)
            (43L,   2, 200)
            (44L,   5, 200)
            (45L,  10, 200)
            (46L,  20, 200)
            (47L,  50, 200)
            (48L, 100, 200)
        ]

    let getDefaultValue (n, s, d) =
        let clmDefaultValueId = (9_000L + n) |> ClmDefaultValueId
        let description = None
        let catRateGenType = ByEnantiomerPairs DistrBased
        let successNumberType = RandomValueBased

        let defaultRateParams =
            //===========================================================
            let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam 0.1
            //===========================================================
            let synthParam = ReactionRateProviderParams.defaultSynthRndParamImpl (Some 0.001, None)
            let catSynthRndParam = (synthParam, (Some ((double s) / 1_000_000.0)), 100_000.0)
            let catSynthParam = ReactionRateProviderParams.defaultCatSynthSimParam catSynthRndParam (Some 0.20) catRateGenType
            //===========================================================
            let destrParam = ReactionRateProviderParams.defaultDestrRndParamImpl (Some 0.001, None)
            let catDestrRndParam = (destrParam, (Some ((double d) / 1_000_000.0)), 100_000.0)
            let catDestrParam = ReactionRateProviderParams.defaultCatDestrSimParam catDestrRndParam (Some 0.20) catRateGenType
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
