namespace ClmDefaults
open Clm.ModelParams

module AllDefaults_002 =
    let defaultValues =
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 50, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 1.0."
            (Defaults_002_000_016.nsd |> List.map Defaults_002_000_016.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 100, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 1.0."
            (Defaults_002_000_018.nsd |> List.map Defaults_002_000_018.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 50, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 0.2."
            (Defaults_002_000_116.nsd |> List.map Defaults_002_000_116.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 100, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 0.2."
            (Defaults_002_000_118.nsd |> List.map Defaults_002_000_118.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 50, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 0.5."
            (Defaults_002_000_216.nsd |> List.map Defaults_002_000_216.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 100, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 0.5."
            (Defaults_002_000_218.nsd |> List.map Defaults_002_000_218.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 50, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 0.001 - 0.05."
            Defaults_002_000_316.defaultValues
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 100, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 0.001 - 0.05."
            Defaults_002_000_318.defaultValues
