namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt
open Clm.Distributions
open ClmSys.ContGenPrimitives
open Clm.ReactionRatesBase

module Defaults_004_005_000 =
    type DefaultDataParam =
        {
            activationScarcity : double
            activationMultiplier : double

            acCatSynthSimilarity : double
            acCatSynthScarcity : double
            acCatSynthMultiplier : double

            acCatDestrSimilarity : double
            acCatDestrScarcity : double
            acCatDestrMultiplier : double

            ligForward : double
            ligBackward : double

            acFwdCatLigSimilarity : double
            acFwdCatLigScarcity : double
            acFwdCatLigMultiplier : double

            acBkwCatLigSimilarity : double
            acBkwCatLigScarcity : double
            acBkwCatLigMultiplier : double

            sugarForward : double
            sugarBackward : double
            sugarScarcity : double
        }

        static member defaultValue =
            {
                activationScarcity = 0.000_100
                activationMultiplier = 100_000.0

                acCatSynthScarcity = 0.000_100
                acCatSynthMultiplier = 100_000.0
                acCatSynthSimilarity = 0.1

                acCatDestrScarcity = 0.000_100
                acCatDestrMultiplier = 100_000.0
                acCatDestrSimilarity = 0.1

                ligForward = 0.001
                ligBackward = 0.010

                acFwdCatLigSimilarity = 0.000_000_001
                acFwdCatLigScarcity = 0.000_000_001
                acFwdCatLigMultiplier = 100_000.0

                acBkwCatLigSimilarity = 0.000_000_001
                acBkwCatLigScarcity = 0.000_000_001
                acBkwCatLigMultiplier = 100_000.0

                sugarForward = 100.0
                sugarBackward = 0.001
                sugarScarcity = 0.001
            }


    let data =
            [
                DefaultDataParam.defaultValue
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

        let description = None
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
            description = description
        }

    let defaultValues =
        printfn "\n"

        data
        |> List.map getDefaultValue
        |> updateDescription "All activated reactions - cat lig with similarity + all sugars playground."
