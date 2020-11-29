namespace ClmDefaults
open Clm.ModelParams

module AllDefaults_001 =

    let defaultValues =
        updateDescription "Catalytic destruction / forward only for n = 20 (catDestrSim = 0.20, vary catDestrScarcity) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_007.nd |> List.map Defaults_001_000_007.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both sim param = 0.20, vary both scarcity param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_009.nsd |> List.map Defaults_001_000_009.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only for n = 20 (catSynthSim = 0.20, vary catSynthScarcity) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_010.ns |> List.map Defaults_001_000_010.getDefaultValue)
        @
        updateDescription "Catalytic destruction / forward only for n = 20 (catDestrScarcity = 20, vary catDestrSim) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_011.nd |> List.map Defaults_001_000_011.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only for n = 20 (catSynthScarcity = 20, vary catSynthSim) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_012.ns |> List.map Defaults_001_000_012.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 50, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_016.nsd |> List.map Defaults_001_000_016.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 20, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_017.nsd |> List.map Defaults_001_000_017.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 100, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_018.nsd |> List.map Defaults_001_000_018.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 200, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_019.nsd |> List.map Defaults_001_000_019.getDefaultValue)

        @
        updateDescription "Catalytic destruction / forward only for n = 20 (catDestrScarcity = 50, vary catDestrSim) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_021.nd |> List.map Defaults_001_000_021.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only for n = 20 (catSynthScarcity = 50, vary catSynthSim) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_022.ns |> List.map Defaults_001_000_022.getDefaultValue)
        @
        updateDescription "Catalytic destruction / forward only for n = 20 (catDestrScarcity = 100, vary catDestrSim) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_023.nd |> List.map Defaults_001_000_023.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only for n = 20 (catSynthScarcity = 100, vary catSynthSim) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_024.ns |> List.map Defaults_001_000_024.getDefaultValue)
        @
        updateDescription "Catalytic destruction / forward only for n = 20 (catDestrScarcity = 200, vary catDestrSim) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_025.nd |> List.map Defaults_001_000_025.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only for n = 20 (catSynthScarcity = 200, vary catSynthSim) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased."
            (Defaults_001_000_026.ns |> List.map Defaults_001_000_026.getDefaultValue)

        @
        Defaults_001_001_018.defaultValues
        @
        Defaults_001_001_022.defaultValues
