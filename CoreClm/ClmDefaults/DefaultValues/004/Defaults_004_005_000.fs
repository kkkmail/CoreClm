namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt
open Clm.Distributions
open ClmSys.ContGenPrimitives
open Clm.ReactionRatesBase

module Defaults_004_005_000 =

    /// Feel free to change the values here in any way as long as they are not used for actual calculations.
    /// Activated catalytic reactions playground.
    type DefaultDataParam =
        {
            activationScarcity : double
            activationMultiplier : double

            acCatSynthScarcity : double
            acCatSynthMultiplier : double
            acCatSynthSimilarity : double

            acCatDestrScarcity : double
            acCatDestrMultiplier : double
            acCatDestrSimilarity : double

            ligForward : double
            ligBackward : double

            acFwdCatLigScarcity : double
            acFwdCatLigMultiplier : double
            acFwdCatLigSimilarity : double

            acBkwCatLigScarcity : double
            acBkwCatLigMultiplier : double
            acBkwCatLigSimilarity : double

            sugarForward : double
            sugarBackward : double
            sugarScarcity : double
        }

        override p.ToString() = $"!!! FOR TESTING PURPOSES ONLY: %0A{p} !!!"

        static member zero =
            {
                activationScarcity = 0.0
                activationMultiplier = 1.0

                acCatSynthScarcity = 0.0
                acCatSynthMultiplier = 1.0
                acCatSynthSimilarity = 0.0

                acCatDestrScarcity = 0.0
                acCatDestrMultiplier = 1.0
                acCatDestrSimilarity = 0.0

                ligForward = 0.001
                ligBackward = 0.010

                acFwdCatLigScarcity = 0.0
                acFwdCatLigMultiplier = 1.0
                acFwdCatLigSimilarity = 0.0

                acBkwCatLigScarcity = 0.0
                acBkwCatLigMultiplier = 1.0
                acBkwCatLigSimilarity = 0.0

                sugarForward = 100.0
                sugarBackward = 0.001
                sugarScarcity = 0.001
            }

        static member zero01 =
            {
                DefaultDataParam.zero
                    with
                    sugarForward = 10.0
            }

        static member zero02 =
            {
                DefaultDataParam.zero
                    with
                    sugarScarcity = 0.002
            }

        static member zero03 =
            {
                DefaultDataParam.zero
                    with
                    sugarScarcity = 0.0005
            }

        /// Use: "ContGenAdm.exe add -i 40050000XX -n 10 -m 3 -y 10 -t 250000 -r 1 -g > -a.txt" for these values.
        /// Adjust XX to match the actual number.
        static member codeGenValue_001 =
            {
//                activationScarcity = 0.001_000
//                activationMultiplier = 100_000.0

                activationScarcity = 1.0
                activationMultiplier = 100_000.0

                acCatSynthScarcity = 0.001_000
                acCatSynthMultiplier = 100_000.0
                acCatSynthSimilarity = 0.1

                acCatDestrScarcity = 0.001_000
                acCatDestrMultiplier = 100_000.0
                acCatDestrSimilarity = 0.1

                ligForward = 0.001
                ligBackward = 0.010

                acFwdCatLigScarcity = 0.000_000_100
                acFwdCatLigMultiplier = 100_000.0
                acFwdCatLigSimilarity = 0.000_000_100

                acBkwCatLigScarcity = 0.000_000_100
                acBkwCatLigMultiplier = 100_000.0
                acBkwCatLigSimilarity = 0.000_000_100

                sugarForward = 100.0
                sugarBackward = 0.001
                sugarScarcity = 0.001
            }

        static member defaultValue =
            {
//                activationScarcity = 0.000_100
//                activationMultiplier = 100_000.0

                activationScarcity = 1.0
                activationMultiplier = 100_000.0

                acCatSynthScarcity = 0.000_050
                acCatSynthMultiplier = 100_000.0
                acCatSynthSimilarity = 0.2

                acCatDestrScarcity = 0.000_050
                acCatDestrMultiplier = 100_000.0
                acCatDestrSimilarity = 0.2

                ligForward = 0.001
                ligBackward = 0.010

                acFwdCatLigScarcity = 0.000_000_002
                acFwdCatLigMultiplier = 100_000.0
                acFwdCatLigSimilarity = 0.000_000_002

                acBkwCatLigScarcity = 0.000_000_002
                acBkwCatLigMultiplier = 100_000.0
                acBkwCatLigSimilarity = 0.000_000_002

                sugarForward = 100.0
                sugarBackward = 0.001
                sugarScarcity = 0.001
            }


    let data =
            [
                DefaultDataParam.zero               // 0
                DefaultDataParam.zero01             // 1
                DefaultDataParam.zero02             // 2
                DefaultDataParam.zero03             // 3

                DefaultDataParam.codeGenValue_001   // 4

                DefaultDataParam.defaultValue       // 5

//                { DefaultDataParam.defaultValue with sugarForward = 10.0 }
//                { DefaultDataParam.defaultValue with acCatLigScarcity = 0.000_000_002 }
//                { DefaultDataParam.defaultValue with sugarForward = 10.0; acCatLigScarcity = 0.000_000_002 }
//
//                { DefaultDataParam.defaultValue with sugarScarcity = 0.002 }
//                { DefaultDataParam.defaultValue with sugarScarcity = 0.000_5 }
//                { DefaultDataParam.defaultValue with sugarScarcity = 0.002; sugarForward = 500.0 }
//                { DefaultDataParam.defaultValue with sugarScarcity = 0.000_5; sugarForward = 500.0 }
//                { DefaultDataParam.defaultValue with sugarScarcity = 0.002; acCatLigSimilarity = 0.002_0 }
//                { DefaultDataParam.defaultValue with sugarScarcity = 0.000_5; acCatLigSimilarity = 0.002_0 }
            ]
            |> withRowNumber


    let getDefaultValue (n, e) =
        let clmDefaultValueId = (4_005_000_000L + n) |> ClmDefaultValueId
        printfn "clmDefaultValueId = %A, e = %A" clmDefaultValueId e

        let description = e.ToString()
        let catRateGenType = ByEnantiomerPairs FixedVal
        let successNumberType = ThresholdBased

        let defaultRateParams =
            //===========================================================
            let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam 0.1
            //===========================================================
            let synthParam = ReactionRateProviderParams.defaultSynthRndParamImpl (Some 0.000_1, Some 0.000_1)

            let acCatSynthRndParam = (synthParam, (Some e.acCatSynthScarcity), e.acCatSynthMultiplier)

            let acCatSynthParam =
                ReactionRateProviderParams.defaultAcCatSynthSimParam acCatSynthRndParam (Some e.acCatSynthSimilarity) catRateGenType
            //===========================================================
            let destrParam = ReactionRateProviderParams.defaultDestrRndParamImpl (Some 0.001, None)

            let acCatDestrRndParam = (destrParam, (Some e.acCatDestrScarcity), e.acCatDestrMultiplier)

            let acCatDestrParam =
                ReactionRateProviderParams.defaultAcCatDestrSimParam acCatDestrRndParam (Some e.acCatDestrSimilarity) catRateGenType
            //===========================================================
            let ligParam = ReactionRateProviderParams.defaultLigRndParamImpl (e.ligForward, e.ligBackward)

            let acFwdCatLigParam =
                ReactionRateProviderParams.defaultAcFwdCatLigSimParam (ligParam, Some (e.acFwdCatLigScarcity), (e.acFwdCatLigMultiplier)) (Some e.acFwdCatLigSimilarity) catRateGenType

            let acBkwCatLigParam =
                ReactionRateProviderParams.defaultAcBkwCatLigSimParam (ligParam, Some (e.acBkwCatLigScarcity), (e.acBkwCatLigMultiplier)) (Some e.acBkwCatLigSimilarity) catRateGenType
            //===========================================================
            let sugParam = ReactionRateProviderParams.defaultSugarSynthRndParamImpl ((Some e.sugarForward, Some e.sugarBackward), Some e.sugarScarcity)
            //===========================================================
            let activationParam = ReactionRateProviderParams.defaultActivationParamImpl (Some e.activationScarcity, e.activationMultiplier)
            //===========================================================
            let rates =
                [
                    wasteRecyclingParam
                    synthParam |> SynthesisRateParam
                    sugParam |> SugarSynthesisRateParam
                    destrParam |> DestructionRateParam
                    ligParam |> LigationRateParam

                    if (e.activationScarcity > 0.0) then activationParam
                    if (e.activationScarcity > 0.0 && e.acCatSynthScarcity > 0.0) then acCatSynthParam
                    if (e.activationScarcity > 0.0 && e.acCatDestrScarcity > 0.0) then acCatDestrParam

                    if (e.activationScarcity > 0.0 && e.acFwdCatLigScarcity > 0.0) then acFwdCatLigParam
                    if (e.activationScarcity > 0.0 && e.acBkwCatLigScarcity > 0.0) then acBkwCatLigParam
                ]
            //===========================================================

            {
                rateParams = rates
                successNumberType = successNumberType
            }

        {
            clmDefaultValueId = clmDefaultValueId
            defaultRateParams = defaultRateParams
            description = Some description
        }

    let defaultValues =
        printfn "\n"

        data
        |> List.map getDefaultValue
        |> updateDescription "All activated reactions - cat lig with similarity + all sugars playground."
