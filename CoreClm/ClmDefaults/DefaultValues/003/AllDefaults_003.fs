namespace ClmDefaults
open Clm.ModelParams

module AllDefaults_003 =
    let defaultValues =
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 50, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 10.0."
            (Defaults_003_000_016.nsd |> List.map Defaults_003_000_016.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 100, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 10.0."
            (Defaults_003_000_018.nsd |> List.map Defaults_003_000_018.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 50, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 2.0."
            (Defaults_003_000_116.nsd |> List.map Defaults_003_000_116.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 100, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 2.0."
            (Defaults_003_000_118.nsd |> List.map Defaults_003_000_118.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 50, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 5.0."
            (Defaults_003_000_216.nsd |> List.map Defaults_003_000_216.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 100, vary both sim param) with catRateGenType = ByEnantiomerPairs FixedVal, successNumberType = ThresholdBased, w = 5.0."
            (Defaults_003_000_218.nsd |> List.map Defaults_003_000_218.getDefaultValue)
