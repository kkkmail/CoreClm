namespace Primitives

open System

module WolframPrimitives =

    let toMathematicaNotation x =
        let rec inner x joiner =
            match box x with
            | :? float as num -> $"{num:E}".Replace("E", "*^")
            | :? seq<_> as seq -> (seq |> Seq.fold (fun acc item -> acc + joiner + (inner item ",\n ")) "{") + "}"
            | _ -> $"{x}"
        inner x ", "

