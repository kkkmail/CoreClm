namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt
open Clm.Distributions
open ClmSys.ContGenPrimitives
open Clm.ReactionRatesBase

module Defaults_004_005_000 =

    /// Activated catalytic reactions playground.
    ///
    /// The value of activationScarcity is set to 1 because we want to activate all "chosen" catalysts
    /// and they are already scarce enough.
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

        override p.ToString() = $"parameters: %0A{p}"

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

        /// Use: "ContGenAdm.exe add -i 4005000004 -n 10 -m 3 -y 10 -t 250000 -r 1 -g > -a.txt" for these values.
        static member codeGenValue_001 =
            {
                activationScarcity = 1.0
                activationMultiplier = 10_000.0

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

        /// Use: "ContGenAdm.exe add -i 4005000006 -n 7 -m 3 -y 10 -t 250000 -r 1 -g > -a.txt" for these values.
        static member codeGenValue_002 =
            {
                activationScarcity = 1.0
                activationMultiplier = 10_000.0

                acCatSynthScarcity = 0.001_000
                acCatSynthMultiplier = 100_000.0
                acCatSynthSimilarity = 0.1

                acCatDestrScarcity = 0.001_000
                acCatDestrMultiplier = 100_000.0
                acCatDestrSimilarity = 0.1

                ligForward = 0.001
                ligBackward = 0.010

                acFwdCatLigScarcity = 0.000_001_000
                acFwdCatLigMultiplier = 100_000.0
                acFwdCatLigSimilarity = 0.000_000_100

                acBkwCatLigScarcity = 0.000_001_000
                acBkwCatLigMultiplier = 100_000.0
                acBkwCatLigSimilarity = 0.000_000_100

                sugarForward = 100.0
                sugarBackward = 0.001
                sugarScarcity = 0.001
            }


        /// Use: "ContGenAdm.exe add -i 4005000005 -n 20 -m 3 -y 10 -t 250000 -r 1 -g > -a.txt" for these values.
        static member defaultValue =
            {
                activationScarcity = 1.0
                activationMultiplier = 10_000.0

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
            let d = DefaultDataParam.defaultValue

            [
                DefaultDataParam.zero               // 0
                DefaultDataParam.zero01             // 1
                DefaultDataParam.zero02             // 2
                DefaultDataParam.zero03             // 3

                DefaultDataParam.codeGenValue_001   // 4

                d                                   // 5
                DefaultDataParam.codeGenValue_002   // 6

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 1.5 }   // 7
                { d with acCatDestrScarcity = d.acCatDestrScarcity * 1.5 }   // 8
                { d with acFwdCatLigScarcity = d.acFwdCatLigScarcity * 1.5 } // 9
                { d with acBkwCatLigScarcity = d.acBkwCatLigScarcity * 1.5 } // 10

                { d with acCatDestrScarcity = d.acCatDestrScarcity * 2.0 }   // 11
                { d with acCatDestrScarcity = d.acCatDestrScarcity * 2.5 }   // 12 +
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.8 }   // 13
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.6 }   // 14

                { d with
                         acCatDestrScarcity = d.acCatDestrScarcity * 2.0
                         acCatSynthScarcity = d.acCatSynthScarcity * 0.8 }   // 15 +

                // ===============================================================

                { d with sugarForward = 0.0 }                                // 16

                // ===============================================================

                { d with
                         acCatDestrScarcity = d.acCatDestrScarcity * 2.0
                         acCatSynthScarcity = d.acCatSynthScarcity * 0.6 }   // 17

                { d with
                         acCatDestrScarcity = d.acCatDestrScarcity * 2.5
                         acCatSynthScarcity = d.acCatSynthScarcity * 0.8 }   // 18

                { d with
                         acCatDestrScarcity = d.acCatDestrScarcity * 2.5
                         acCatSynthScarcity = d.acCatSynthScarcity * 0.6 }   // 19

                // ===============================================================

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.5 }   // 20
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.4 }   // 21
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0 }   // 22

                { d with
                         acCatDestrScarcity = d.acCatDestrScarcity * 3.0
                         acCatSynthScarcity = d.acCatSynthScarcity * 0.5 }   // 23

                // ===============================================================

                { d with acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.8 } // 24
                { d with acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.8 } // 25

                { d with
                         acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 1.5
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.8 } // 26

                { d with
                         acCatDestrScarcity = d.acCatDestrScarcity * 3.0
                         acCatSynthScarcity = d.acCatSynthScarcity * 0.5
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 1.5
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.8 } // 27

                { d with acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.6 } // 28
                { d with acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.6 } // 29

                { d with
                         acCatDestrScarcity = d.acCatDestrScarcity * 3.0
                         acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 1.5
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.8 } // 30

                // ===============================================================
            ]
            |> withRowNumber


    let getDefaultValue (n, e) =
        let clmDefaultValueId = (4_005_000_000L + n) |> ClmDefaultValueId
        printfn $"clmDefaultValueId = %A{clmDefaultValueId}, e = %A{e}"

        let description = e.ToString()
        let catRateGenType = ByEnantiomerPairs FixedVal
        let successNumberType = ThresholdBased

        let defaultRateParams =
            //===========================================================
            let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam 0.1
            //===========================================================
            let synthParam = ReactionRateProviderParams.defaultSynthRndParamImpl (Some 0.001, Some 0.001)

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
                ReactionRateProviderParams.defaultAcFwdCatLigSimParam (ligParam, Some e.acFwdCatLigScarcity, e.acFwdCatLigMultiplier) (Some e.acFwdCatLigSimilarity) catRateGenType

            let acBkwCatLigParam =
                ReactionRateProviderParams.defaultAcBkwCatLigSimParam (ligParam, Some e.acBkwCatLigScarcity, e.acBkwCatLigMultiplier) (Some e.acBkwCatLigSimilarity) catRateGenType
            //===========================================================
            let sugParam = ReactionRateProviderParams.defaultSugarSynthRndParamImpl ((Some e.sugarForward, Some e.sugarBackward), Some e.sugarScarcity)
            //===========================================================
            let activationParam = ReactionRateProviderParams.defaultActivationParamImpl (Some e.activationScarcity, e.activationMultiplier)
            //===========================================================
            let rates =
                [
                    wasteRecyclingParam
                    synthParam |> SynthesisRateParam
                    destrParam |> DestructionRateParam
                    ligParam |> LigationRateParam

                    if e.sugarForward > 0.0
                    then
                        sugParam |> SugarSynthesisRateParam

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
