namespace Primitives

open Primitives.GeneralData
open System
open Microsoft.FSharp.Reflection

module WolframPrimitives =

    //let toWolframNotation v =
    //    let rec inner x joiner =
    //        match box x with
    //        | :? float as num -> $"{num:E}".Replace("E", "*^")
    //        | :? seq<_> as seq -> (seq |> Seq.fold (fun acc item -> acc + joiner + (inner item ",\n ")) "{") + "}"
    //        | _ when FSharpType.IsRecord(x.GetType()) ->
    //            let propertyValues =
    //                FSharpType.GetRecordFields(x.GetType())
    //                |> Array.sortBy (fun fi -> fi.Name)
    //                |> Array.map (fun fi -> fi.GetValue(x))

    //            //let g v =
    //            //    let u = unbox v
    //            //    let r = inner u ", "
    //            //    r

    //            let g (v: obj) =
    //                match v with
    //                | :? float as num -> inner num ", "
    //                | _ -> inner v ", "

    //            propertyValues
    //            |> Array.map g
    //            |> fun arr -> "{ " + String.Join(", ", arr) + " }"
    //        | _ -> $"{x}"

    //    inner v ", "

    //let toWolframNotation v =
    //    let rec inner (x: obj) joiner =
    //        match x with
    //        | :? float as num -> $"{num:E}".Replace("E", "*^")
    //        | _ when FSharpType.IsRecord(x.GetType()) ->
    //            let propertyValues =
    //                FSharpType.GetRecordFields(x.GetType())
    //                |> Array.sortBy (fun fi -> fi.Name)
    //                |> Array.map (fun fi -> fi.GetValue(x, null))

    //            propertyValues
    //            |> Array.map (fun v -> inner v ", ")
    //            |> fun arr -> "{ " + String.Join(", ", arr) + " }"
    //        | :? seq<_> as seq -> (seq |> Seq.fold (fun acc item -> acc + joiner + (inner item ",\n ")) "{") + "}"
    //        | _ -> $"{x}"

    //    inner (box v) ", "


    //let toWolframNotation v =
    //    // Helper function to check if object is a sequence but not a string
    //    let isSeq obj =
    //        if obj = null then false
    //        elif obj.GetType().GetInterface("System.Collections.IEnumerable") <> null && 
    //             obj.GetType() <> typeof<string> then true
    //        else false

    //    let rec inner (x: obj) joiner =
    //        match x with
    //        | :? float as num -> $"{num:E}".Replace("E", "*^")
    //        | _ when FSharpType.IsRecord(x.GetType()) ->
    //            let propertyValues =
    //                FSharpType.GetRecordFields(x.GetType())
    //                |> Array.sortBy (fun fi -> fi.Name)
    //                |> Array.map (fun fi -> fi.GetValue(x, null))

    //            propertyValues
    //            |> Array.map (fun v -> inner v ", ")
    //            |> fun arr -> "{ " + String.Join(", ", arr) + " }"
    //        | _ when isSeq x ->
    //            let seq = x :?> System.Collections.IEnumerable |> Seq.cast<_>
    //            (seq |> Seq.fold (fun acc item -> acc + joiner + (inner item ",\n ")) "{") + "}"
    //        | _ -> $"{x}"

    //    inner (box v) ", "


    //let toWolframNotation v =
    //    // Helper function to check if object is a sequence but not a string
    //    let isSeq obj =
    //        if obj = null then false
    //        elif obj.GetType().GetInterface(typeof<System.Collections.IEnumerable>.FullName) <> null && 
    //             obj.GetType() <> typeof<string> then true
    //        else false

    //    let rec inner (x: obj) joiner =
    //        match x with
    //        | :? float as num -> $"{num:E}".Replace("E", "*^")
    //        | _ when FSharpType.IsRecord(x.GetType()) ->
    //            let propertyValues =
    //                FSharpType.GetRecordFields(x.GetType())
    //                |> Array.sortBy (fun fi -> fi.Name)
    //                |> Array.map (fun fi -> fi.GetValue(x, null))

    //            propertyValues
    //            |> Array.map (fun v -> inner v ", ")
    //            |> fun arr -> "{ " + String.Join(", ", arr) + " }"
    //        | _ when isSeq x ->
    //            let seq = x :?> System.Collections.IEnumerable |> Seq.cast<_>
    //            (seq |> Seq.fold (fun acc item -> acc + joiner + (inner item ",\n ")) "{") + "}"
    //        | _ -> $"{x}"

    //    inner (box v) ", "

    //let toWolframNotation v =
    //    // Helper function to check if object is a sequence but not a string
    //    let isSeq obj =
    //        if obj = null then false
    //        elif obj.GetType().GetInterface(typeof<System.Collections.IEnumerable>.FullName) <> null && 
    //             obj.GetType() <> typeof<string> then true
    //        else false

    //    let rec inner (x: obj) joiner =
    //        match x with
    //        | :? float as num -> $"{num:E}".Replace("E", "*^")
    //        | _ when FSharpType.IsRecord(x.GetType()) ->
    //            let propertyValues =
    //                FSharpType.GetRecordFields(x.GetType())
    //                |> Array.sortBy (fun fi -> fi.Name)
    //                |> Array.map (fun fi -> fi.GetValue(x, null))

    //            propertyValues
    //            |> Array.map (fun v -> inner v ", ")
    //            |> fun arr -> "{ " + String.Join(", ", arr) + " }"
    //        | _ when isSeq x ->
    //            let seq = x :?> System.Collections.IEnumerable |> Seq.cast<_>
    //            let convertedSeq = seq |> Seq.map (fun item -> inner item ",\n ")
    //            "{ " + String.Join(", ", convertedSeq) + " }"
    //        | _ -> $"{x}"

    //    inner (box v) ", "

    //let toWolframNotation v =
    //    // Helper function to check if object is a sequence but not a string
    //    let isSeq obj =
    //        if obj = null then false
    //        elif obj.GetType().GetInterface(typeof<System.Collections.IEnumerable>.FullName) <> null && 
    //             obj.GetType() <> typeof<string> then true
    //        else false

    //    let rec inner (x : obj) =
    //        match x with
    //        | :? float as num -> $"{num:E}".Replace("E", "*^")
    //        | _ when FSharpType.IsRecord(x.GetType()) ->
    //            let propertyValues =
    //                FSharpType.GetRecordFields(x.GetType())
    //                |> Array.sortBy (fun fi -> fi.Name)
    //                |> Array.map (fun fi -> fi.GetValue(x, null))

    //            propertyValues
    //            |> Array.map inner
    //            |> fun arr -> "{ " + String.Join(", ", arr) + " }"
    //        | _ when isSeq x ->
    //            let seq = x :?> System.Collections.IEnumerable |> Seq.cast<_>
    //            let convertedSeq = seq |> Seq.map inner
    //            "{ " + String.Join(", ", convertedSeq) + " }"
    //        | _ -> $"{x}"

    //    inner (box v)

    let toWolframNotation v =
        // Helper function to check if object is a sequence but not a string
        let isSeq obj =
            if obj = null then false
            elif obj.GetType().GetInterface(typeof<System.Collections.IEnumerable>.FullName) <> null && 
                 obj.GetType() <> typeof<string> then true
            else false

        let rec inner (x : obj) =
            match x with
            | :? float as num -> $"{num:E}".Replace("E", "*^")
            | _ when FSharpType.IsRecord(x.GetType()) ->
                let propertyValues =
                    FSharpType.GetRecordFields(x.GetType())
                    |> Array.sortBy (fun fi -> fi.Name)
                    |> Array.map (fun fi -> fi.GetValue(x, null))

                propertyValues
                |> Array.map inner
                |> fun arr -> "{ " + String.Join(", ", arr) + " }"
            | _ when isSeq x ->
                let seq = x :?> System.Collections.IEnumerable |> Seq.cast<_>
                let convertedSeq = seq |> Seq.map inner
                "{ " + String.Join(", ", convertedSeq) + " }"
            | _ -> $"{x}"

        inner (box v)

