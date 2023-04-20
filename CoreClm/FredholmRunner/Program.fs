open System
open System.Diagnostics
open FredholmSolver.Primitives
open FredholmSolver.Kernel

printfn "Starting..."
let sw = Stopwatch.StartNew()

// let data =
//     {
//         noOfIntervals = 100
//         l2 = 25
//         zeroThreshold = MutationProbabilityData.defaultZeroThreshold
//         epsEeFunc = (fun _ -> 0.02) |> EpsFunc
//         epsInfFunc = (fun _ -> 0.02) |> EpsFunc
//         kaFunc = (fun _ _ _ -> 1.0) |> KaFunc
//     }
//
// let domain = Domain2D.create data.noOfIntervals data.l2
//
// let m2Data =
//     {
//         eeMutationProbabilityData =
//             {
//                 domain = domain.eeDomain
//                 zeroThreshold = MutationProbabilityData.defaultZeroThreshold
//                 epsFunc = data.epsEeFunc
//             }
//         infMutationProbabilityData =
//             {
//                 domain = domain.infDomain
//                 zeroThreshold = MutationProbabilityData.defaultZeroThreshold
//                 epsFunc = data.epsInfFunc
//             }
//     }
//
// printfn $"domain.eeDomain: {domain.eeDomain}"
// printfn $"domain.infDomain: {domain.infDomain}"
//
// // let mpEe = MutationProbability.create domain.eeDomain MutationProbability.defaultZeroValue 0.0 data.epsEe
// // let mpInf = MutationProbability.create domain.infDomain MutationProbability.defaultZeroValue 0.0 data.epsInf
// // printfn $"Elapsed {nameof(mpEe)}: {sw.Elapsed.TotalSeconds}."
// //
// // let mpEeInt = domain.eeDomain.integrateValues mpEe.p
// // printfn $"mpEeInt = {mpEeInt}, mpEe: {mpEe}"
// //
// // let mpInfInt = domain.infDomain.integrateValues mpInf.p
// // printfn $"mpInfInt = {mpInfInt}, mpInf: {mpInf}"
// //
// // let mpEeInf = cartesianMultiply mpEe.p mpInf.p
// // let mpEeInfInt = domain.integrateValues mpEeInf
// // printfn $"mpEeInfInt = {mpEeInfInt}, mpEeInf: {mpEeInf}"
//
// let p = MutationProbability4D.create m2Data
//
// printfn $"{sw.Elapsed.TotalSeconds}."
// // printfn $"p.xy_x1y1 = {SparseArray4D.create p.xy_x1y1}"
// // printfn $"p.x1y1_xy = {SparseArray4D.create p.x1y1_xy}"
//
// let x1 = domain.integrateValues p.x1y1_xy
//
// let x2 = domain.integrateValues p.xy_x1y1
//
// printfn $"integrate(p.x1y1_xy) = {x1}"
// printfn $"integrate(p.xy_x1y1) = {x2}"
// printfn $"{sw.Elapsed.TotalSeconds}."

Console.ReadLine() |> ignore
