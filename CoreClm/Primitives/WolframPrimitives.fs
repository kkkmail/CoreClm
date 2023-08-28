namespace Primitives

open Primitives.GeneralData
open System
open Microsoft.FSharp.Reflection

module WolframPrimitives =

    let toWolframNotation v =
        let rec inner x joiner =
            match box x with
            | :? float as num -> $"{num:E}".Replace("E", "*^")
            | :? seq<_> as seq -> (seq |> Seq.fold (fun acc item -> acc + joiner + (inner item ",\n ")) "{") + "}"
            | _ when FSharpType.IsRecord(x.GetType()) ->
                let propertyValues =
                    FSharpType.GetRecordFields(x.GetType())
                    |> Array.sortBy (fun fi -> fi.Name)
                    |> Array.map (fun fi -> fi.GetValue(x))

                propertyValues
                |> Array.map (fun value -> inner (unbox value) ", ")
                |> fun arr -> "{ " + String.Join(", ", arr) + " }"
            | _ -> $"{x}"

        inner v ", "

