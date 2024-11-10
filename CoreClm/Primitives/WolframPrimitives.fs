namespace Primitives

open System
open Microsoft.FSharp.Reflection
open Primitives.GeneralData
open System.Text
open Softellect.Sys.Primitives

module WolframPrimitives =
    let x = 1

    //let private baseIndent = "  "
    //let joinStrings j (s : #seq<'T>) = String.Join(j, s |> Seq.map (fun e -> $"{e}"))
    //let isDiscriminatedUnion obj = FSharpType.IsUnion(obj.GetType())
    //let isRecord obj = FSharpType.IsRecord(obj.GetType())
    //let isArray obj = obj <> null && obj.GetType().IsArray
    //let isTuple obj = obj <> null && FSharpType.IsTuple(obj.GetType())


    ///// Helper function to check if object is a sequence but not a string
    //let isSeq obj =
    //    if obj = null then false
    //    elif obj.GetType().GetInterface(typeof<System.Collections.IEnumerable>.FullName) <> null &&
    //         obj.GetType() <> typeof<string> then true
    //    else false


    //let isList obj =
    //    obj <> null && obj.GetType().IsGenericType &&
    //    obj.GetType().GetGenericTypeDefinition() = typedefof<List<_>>


    //let isSimpleType obj =
    //    obj <> null &&
    //    not (isDiscriminatedUnion obj || isRecord obj) &&
    //    (obj.GetType().IsPrimitive || obj.GetType() = typeof<string> || obj.GetType() = typeof<decimal> || obj.GetType() = typeof<DateTime> || isTuple obj)


    //let toWolframNotation v =
    //    // Helper function to get depth of sequences and records
    //    let rec seqDepth currentDepth (x : obj) =
    //        match x with
    //        | _ when isSeq x ->
    //            let seq = x :?> System.Collections.IEnumerable |> Seq.cast<_>
    //            if Seq.isEmpty seq then currentDepth + 1
    //            else Seq.map (seqDepth (currentDepth + 1)) seq |> Seq.max
    //        | _ when FSharpType.IsRecord(x.GetType()) ->
    //            let propertyValues =
    //                FSharpType.GetRecordFields(x.GetType())
    //                |> Array.map (fun fi -> fi.GetValue(x, null))
    //            Array.map (seqDepth currentDepth) propertyValues |> Array.max
    //        | _ -> currentDepth

    //    let rec inner (nestingLevel : int) (maxDepth: int) (x : obj) =
    //        let seqSeparator =
    //            if nestingLevel = (maxDepth - 1) then ", "
    //            else $",{Nl}"

    //        match x with
    //        | :? float as num -> $"{num:E}".Replace("E", "*^")
    //        | :? string as s -> s.Replace("\"", "\\\"")
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


    //let getBrackets obj =
    //    match obj with
    //    | _ when isArray obj -> ("[|", "|]")
    //    | _ when isList obj -> ("[", "]")
    //    | _ -> ("{{", "}}")


    //let getComplexSeparator obj =
    //    match obj with
    //    | _ when FSharpType.IsRecord(obj.GetType()) -> ""
    //    | _ when isArray obj -> ""
    //    | _ when isList obj -> ""
    //    | _ when isSeq obj -> ""
    //    | _ -> " "


    //let toOutputString (x: obj) : string =
    //    let rec inner (x: obj) (indent: string) : string =
    //        match x with
    //        | null -> "null"
    //        | :? string as str -> $"\"{str}\""
    //        | _ when isTuple x ->
    //            let elements = FSharpValue.GetTupleFields(x) |> Array.map (fun el -> inner el "")
    //            $"""({String.Join(", ", elements)})"""
    //        | _ when isArray x || isList x || isSeq x ->
    //            let brackets = getBrackets x
    //            let elements = (x :?> System.Collections.IEnumerable) |> Seq.cast<obj> |> Seq.toList
    //            if List.forall isSimpleType elements then $""" {(fst brackets)} {String.Join("; ", elements)} {(snd brackets)}"""
    //            else
    //                let newIndent = $"{baseIndent}{indent}"
    //                let sb = StringBuilder()
    //                sb.AppendLine() |> ignore
    //                sb.Append($"{baseIndent}{indent}{(fst brackets)}") |> ignore
    //                let formattedElems = elements |> List.map (fun el -> $"{inner el newIndent}")

    //                if List.forall isDiscriminatedUnion elements
    //                then
    //                    sb.AppendLine() |> ignore
    //                    sb.Append($"{baseIndent}{newIndent}") |> ignore
    //                    sb.AppendLine(String.Join($"{Nl}{baseIndent}{newIndent}", formattedElems)) |> ignore
    //                else
    //                    sb.AppendLine(String.Join("", formattedElems)) |> ignore

    //                sb.Append($"{baseIndent}{indent}{(snd brackets)}") |> ignore
    //                sb.ToString()
    //        | _ when FSharpType.IsRecord(x.GetType()) ->
    //            let sb = StringBuilder()
    //            if indent <> "" then sb.AppendLine("") |> ignore
    //            let indent1 = if indent <> "" then $"{baseIndent}{indent}" else indent
    //            sb.AppendLine($"{indent1}{{") |> ignore

    //            let recordType = x.GetType()
    //            let fields = FSharpType.GetRecordFields(recordType)
    //            let newIndent = $"{baseIndent}{indent1}"

    //            for field in fields do
    //                let fieldName = field.Name
    //                let fieldValue = field.GetValue(x)
    //                let separator = getComplexSeparator fieldValue
    //                sb.AppendLine($"{newIndent}{fieldName} ={separator}{inner fieldValue newIndent}") |> ignore

    //            sb.Append($"{indent1}}}") |> ignore
    //            sb.ToString()
    //        | _ when FSharpType.IsUnion(x.GetType()) ->
    //            let unionType = x.GetType()
    //            let case, fields = FSharpValue.GetUnionFields(x, unionType)
    //            let caseName = case.Name
    //            let fieldStrs = fields |> Array.map (fun f -> inner f indent)
    //            let allFieldsStr = $"""{String.Join(" ", fieldStrs)}"""

    //            let sep =
    //                if allFieldsStr <> "" && allFieldsStr.StartsWith " " |> not then " "
    //                else ""

    //            $"""{caseName}{sep}{allFieldsStr}"""
    //        | _ -> x.ToString()
    //    inner x ""
