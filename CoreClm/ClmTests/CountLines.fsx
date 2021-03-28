﻿// http://www.fssnip.net/b6/title/How-many-lines-of-code-does-your-project-contain

open System
open System.IO

let countLines path wildcard recurse =

    let lineCount file =

        let isEmpty (line : string) = line.Trim() = ""
        let isComment (line : string) = line.Trim().StartsWith("//")
        let isCode (line : string) = not (isEmpty line) && not (isComment line)

        File.ReadAllLines file
        |> Seq.filter isCode
        |> Seq.length

    Directory.EnumerateFiles(path, wildcard, if recurse then SearchOption.AllDirectories else SearchOption.TopDirectoryOnly)
    |> Seq.map lineCount
    |> Seq.sum

let src = __SOURCE_DIRECTORY__
printfn $"{src}"

let lines = countLines @"C:\GitHub\CoreClm\CoreClm" "*.fs" true
let modelLines = countLines @"C:\GitHub\CoreClm\CoreClm\Model" "*.fs" true

printfn $"lines = {lines}, modelLines = {modelLines}, actual = {lines - modelLines}."

