// http://www.fssnip.net/b6/title/How-many-lines-of-code-does-your-project-contain

open System
open System.IO

let countLines path wildcard recurse =
    printfn $"Counting '{wildcard}' lines in '{path}'."

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
printfn $"Script folder: '{src}'."

let lines = countLines @$"{src}\.." "*.fs" true
let modelLines = countLines @$"{src}\..\Model" "*.fs" true
printfn $"lines = {lines}, modelLines = {modelLines}, actual = {lines - modelLines}."

let propulsionLines = countLines @"C:\GitHub\propulsion" "*.fs" true
let equinoxLines = countLines @"C:\GitHub\equinox"  "*.fs" true
let softellectLines = countLines @"C:\GitHub\Softellect"  "*.fs" true
printfn $"propulsionLines = {propulsionLines}, equinoxLines = {equinoxLines}, softellectLines = {softellectLines}."
