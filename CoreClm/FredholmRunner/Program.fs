open System
open System.Diagnostics
open FredholmRunner.FredholmData

printfn "Starting..."
let sw = Stopwatch.StartNew()

let data =
    {
        noOfIntervals = 100
        l2 = 25
        epsEe = 0.02
        epsInf = 0.02
    }

// let kernel

let domain = DomainData.create data.noOfIntervals data.l2

printfn $"domain.eeDomain: {domain.eeDomain}"
printfn $"domain.infDomain: {domain.infDomain}"

let mpEe = MutationProbability.create domain.eeDomain MutationProbability.defaultZeroValue 0.0 data.epsEe
let mpInf = MutationProbability.create domain.infDomain MutationProbability.defaultZeroValue 0.0 data.epsInf
printfn $"Elapsed {nameof(mpEe)}: {sw.Elapsed.TotalSeconds}."

let mpEeInt = domain.eeDomain.integrateValues mpEe.p
printfn $"mpEeInt = {mpEeInt}, mpEe: {mpEe}"

let mpInfInt = domain.infDomain.integrateValues mpInf.p
printfn $"mpInfInt = {mpInfInt}, mpInf: {mpInf}"

let mpEeInf = SparseValue2D.cartesianMultiply mpEe.p mpInf.p
let mpEeInfInt = domain.integrateValues mpEeInf
printfn $"mpEeInfInt = {mpEeInfInt}, mpEeInf: {mpEeInf}"

Console.ReadLine() |> ignore
