namespace ClmDefaults

open AllDefaults
open DbData.Configuration
open DbData.DatabaseTypes
open Clm.ModelParams

module ClmDefaultsTasks =

    let saveAllDefaults() =
        printfn "saveAllDefaults: Saving all defaults."

        let saveDefault (_, (d : ClmDefaultValue)) =
            printfn $"    %A{d.clmDefaultValueId}"

            match upsertClmDefaultValue getContGenConnectionString d with
            | Ok() -> ()
            | Error e -> printfn $"saveAllDefaults: Error - {e}"

        defaultValues
        |> Map.toList
        |> List.map saveDefault
        |> ignore

        printfn "Completed."
