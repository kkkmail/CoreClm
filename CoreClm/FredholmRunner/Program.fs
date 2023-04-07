open System
open System.Diagnostics
open FredholmRunner.FredholmData

printfn "Starting..."
let sw = Stopwatch.StartNew()

let data =
    {
        noOfIntervals = 100
        l2 = 25
        epsEe = fun _ -> 0.02
        epsInf = fun _ -> 0.02
    }

let domain = Domain2D.create data.noOfIntervals data.l2

let m2Data =
    {
        eeMutationProbabilityData =
            {
                domain = domain.eeDomain
                zeroThreshold = MutationProbabilityData.defaultZeroThreshold
                epsFunc = data.epsEe
            }
        infMutationProbabilityData =
            {
                domain = domain.infDomain
                zeroThreshold = MutationProbabilityData.defaultZeroThreshold
                epsFunc = data.epsInf
            }
    }

printfn $"domain.eeDomain: {domain.eeDomain}"
printfn $"domain.infDomain: {domain.infDomain}"

// let mpEe = MutationProbability.create domain.eeDomain MutationProbability.defaultZeroValue 0.0 data.epsEe
// let mpInf = MutationProbability.create domain.infDomain MutationProbability.defaultZeroValue 0.0 data.epsInf
// printfn $"Elapsed {nameof(mpEe)}: {sw.Elapsed.TotalSeconds}."
//
// let mpEeInt = domain.eeDomain.integrateValues mpEe.p
// printfn $"mpEeInt = {mpEeInt}, mpEe: {mpEe}"
//
// let mpInfInt = domain.infDomain.integrateValues mpInf.p
// printfn $"mpInfInt = {mpInfInt}, mpInf: {mpInf}"
//
// let mpEeInf = cartesianMultiply mpEe.p mpInf.p
// let mpEeInfInt = domain.integrateValues mpEeInf
// printfn $"mpEeInfInt = {mpEeInfInt}, mpEeInf: {mpEeInf}"

let p = MutationProbability4D.create m2Data

printfn $"{sw.Elapsed.TotalSeconds}."
// printfn $"p.xy_x1y1 = {SparseArray4D.create p.xy_x1y1}"
// printfn $"p.x1y1_xy = {SparseArray4D.create p.x1y1_xy}"

let x1 =
    p.x1y1_xy
    |> Array.map (fun a -> a |> Array.map domain.integrateValues)
    |> XY

let x2 =
    p.xy_x1y1
    |> Array.map (fun a -> a |> Array.map domain.integrateValues)
    |> XY

printfn $"integrate(p.x1y1_xy) = {x1}"
printfn $"integrate(p.xy_x1y1) = {x2}"
printfn $"{sw.Elapsed.TotalSeconds}."

Console.ReadLine() |> ignore
