namespace ClmDefaults
open Clm.ModelParams

module AllDefaults_000 =

    let defaultValues =
        updateDescription "Catalytic destruction for n = 20." [ Defaults_000_000.defaultValue ]
        @
        updateDescription "Catalytic destruction / forward only for n = 20 (catDestrSim = 0.20, vary catDestrScarcity)."
            (Defaults_000_007.nd |> List.map Defaults_000_007.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both sim param = 0.20, vary both scarcity param)."
            (Defaults_000_009.nsd |> List.map Defaults_000_009.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only for n = 20 (catSynthSim = 0.20, vary catSynthScarcity)."
            (Defaults_000_010.ns |> List.map Defaults_000_010.getDefaultValue)
        @
        updateDescription "Catalytic destruction / forward only for n = 20 (catDestrScarcity = 20, vary catDestrSim)."
            (Defaults_000_011.nd |> List.map Defaults_000_011.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only for n = 20 (catSynthScarcity = 20, vary catSynthSim)."
            (Defaults_000_012.ns |> List.map Defaults_000_012.getDefaultValue)
        @
        updateDescription "Catalytic destruction / forward only for n = 20 (catDestrScarcity = 20, catDestrSim = 0.20, vary wasteRecyclingRate)."
            (Defaults_000_013.nw |> List.map Defaults_000_013.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only for n = 20 (catSynthScarcity = 20, catSynthSim = 0.20, vary wasteRecyclingRate)."
            (Defaults_000_014.nw |> List.map Defaults_000_014.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both sim param = 0.20, vary both scarcity param), wasteRecyclingRate upped to 10.0."
            (Defaults_000_015.nsd |> List.map Defaults_000_015.getDefaultValue)
        @
        updateDescription "Catalytic synthesis / forward only + catalytic destruction / forward only for n = 20 (both scarcity param = 50, vary both sim param)."
            (Defaults_000_016.nsd |> List.map Defaults_000_016.getDefaultValue)


        @
        updateDescription "Tests." [ Defaults_999_999.defaultValue ]

