namespace Primitives

open System
open Microsoft.FSharp.Reflection
open Primitives.GeneralData

module WolframPrimitives =

    let joinStrings j s = String.Join(j, s |> List.map (fun e -> $"{e}"))


    //let toWolframNotation v =
    //    // Helper function to check if object is a sequence but not a string
    //    let isSeq obj =
    //        if obj = null then false
    //        elif obj.GetType().GetInterface(typeof<System.Collections.IEnumerable>.FullName) <> null &&
    //             obj.GetType() <> typeof<string> then true
    //        else false

    //    // Helper function to get depth of sequences and records
    //    let rec seqDepth currentDepth (x : obj) =
    //        if isSeq x then
    //            let seq = x :?> System.Collections.IEnumerable |> Seq.cast<_>
    //            if Seq.isEmpty seq then currentDepth + 1
    //            else Seq.map (seqDepth (currentDepth + 1)) seq |> Seq.max
    //        elif FSharpType.IsRecord(x.GetType()) then
    //            let propertyValues =
    //                FSharpType.GetRecordFields(x.GetType())
    //                |> Array.map (fun fi -> fi.GetValue(x, null))
    //            Array.map (seqDepth currentDepth) propertyValues |> Array.max
    //        else currentDepth

    //    let rec inner (nestingLevel : int) (maxDepth: int) (x : obj) =
    //        let seqSeparator =
    //            if nestingLevel = (maxDepth - 1) then ", "
    //            else $",{System.Environment.NewLine}"

    //        match x with
    //        | :? float as num -> $"{num:E}".Replace("E", "*^")
    //        | _ when FSharpType.IsRecord(x.GetType()) ->
    //            let propertyValues =
    //                FSharpType.GetRecordFields(x.GetType())
    //                |> Array.sortBy (fun fi -> fi.Name)
    //                |> Array.map (fun fi -> fi.GetValue(x, null))

    //            propertyValues
    //            |> Array.map (inner nestingLevel maxDepth)
    //            |> fun arr -> "{ " + String.Join(", ", arr) + " }"
    //        | _ when isSeq x ->
    //            let seq = x :?> System.Collections.IEnumerable |> Seq.cast<_>
    //            let localMaxDepth = seqDepth 0 x
    //            let convertedSeq = seq |> Seq.map (inner (nestingLevel + 1) localMaxDepth)
    //            "{ " + String.Join(seqSeparator, convertedSeq) + " }"
    //        | _ -> $"{x}"

    //    let maxDepth = seqDepth 0 (box v)
    //    inner 0 maxDepth (box v)

    let toWolframNotation v =
        // Helper function to check if object is a sequence but not a string
        let isSeq obj =
            if obj = null then false
            elif obj.GetType().GetInterface(typeof<System.Collections.IEnumerable>.FullName) <> null &&
                 obj.GetType() <> typeof<string> then true
            else false

        // Helper function to get depth of sequences and records
        let rec seqDepth currentDepth (x : obj) =
            match x with
            | _ when isSeq x ->
                let seq = x :?> System.Collections.IEnumerable |> Seq.cast<_>
                if Seq.isEmpty seq then currentDepth + 1
                else Seq.map (seqDepth (currentDepth + 1)) seq |> Seq.max
            | _ when FSharpType.IsRecord(x.GetType()) ->
                let propertyValues =
                    FSharpType.GetRecordFields(x.GetType())
                    |> Array.map (fun fi -> fi.GetValue(x, null))
                Array.map (seqDepth currentDepth) propertyValues |> Array.max
            | _ -> currentDepth

        let rec inner (nestingLevel : int) (maxDepth: int) (x : obj) =
            let seqSeparator =
                if nestingLevel = (maxDepth - 1) then ", "
                else $",{System.Environment.NewLine}"

            match x with
            | :? float as num -> $"{num:E}".Replace("E", "*^")
            | _ when FSharpType.IsRecord(x.GetType()) ->
                let propertyValues =
                    FSharpType.GetRecordFields(x.GetType())
                    |> Array.sortBy (fun fi -> fi.Name)
                    |> Array.map (fun fi -> fi.GetValue(x, null))

                propertyValues
                |> Array.map (inner nestingLevel maxDepth)
                |> fun arr -> "{ " + String.Join(", ", arr) + " }"
            | _ when isSeq x ->
                let seq = x :?> System.Collections.IEnumerable |> Seq.cast<_>
                let localMaxDepth = seqDepth 0 x
                let convertedSeq = seq |> Seq.map (inner (nestingLevel + 1) localMaxDepth)
                "{ " + String.Join(seqSeparator, convertedSeq) + " }"
            | _ -> $"{x}"

        let maxDepth = seqDepth 0 (box v)
        inner 0 maxDepth (box v)
