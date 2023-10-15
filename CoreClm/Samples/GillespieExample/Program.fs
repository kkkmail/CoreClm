open System
open Plotly.NET

open Softellect.Sys.Core
open Gillespie.SsaPrimitives
open Gillespie.LotkaVolterra
open MathNet.Numerics.Distributions

//open Gillespie.SsaSolver
open Gillespie.SsaSolverMutable
open System.Diagnostics

let noOfPoinssonSamles = 1_000_000
let lambda = 2.0e12
//let lambda = (float (Int32.MaxValue)) - 10.0 * (sqrt(float (Int32.MaxValue)))

printfn $"Sampling for lambda = {lambda} ..."
let rnd = new Random()
let sw = Stopwatch.StartNew()
let x = [| for _ in 1 .. noOfPoinssonSamles -> poissonSample rnd lambda |]
printfn $"Sampling {noOfPoinssonSamles} one by one took: {sw.Elapsed.TotalSeconds} seconds."
printfn $"Average = {(x |> Array.map float |> Array.average)}."

//sw.Restart()
//Poisson.Samples(x, lambda)
//printfn $"Sampling {noOfPoinssonSamles} together took: {sw.Elapsed.TotalSeconds} seconds."
//printfn $"Average = {(x |> Array.map float |> Array.average)}."

let noOfSteps = 1_000_000
//let noOfSteps = 1_000
let noOfFoxes = 10
let noOfHares = 1_000

let populations =
    {
        noOfHares = 1_000
        noOfFoxes = 10
    }

let rates =
    {
        haresMultiplyRate = 1.0 // Hares multiply: dx / dt = a * x
        haresEatenRate = 0.03 // Hares got eaten: dx / dt = -b * x * y
        foxesDieOffRate = 1.0 // Foxes die off: dy / dt = -c * x
        foxesMultiplyRate = 0.01 // Foxes multiply: dy / dt = d * x * y
        haresResurrectRate = 0.1 // Hares resurrect: dx / dt = e
        foxesResurrectRate = 0.1 // Foxes resurrect: dx / dt = f
        haresDieOffRate = 0.000_1 // Hares die from overpopulation: dx / dt = -g * x * x
    }


let v0 = StateVector.create (createSpecies populations) (createReactions rates)
let r1 = Random(1)
let r2 = Random(2)


let steps = [ for i in 0..noOfSteps -> i ]



let getData i v =
    {
        t = v.time
        foxes = (getValueOrDefault v.state fox NoOfMolecules.zero).value
        hares = (getValueOrDefault v.state hare NoOfMolecules.zero).value
        step = i
    }

//printfn $"v0: %A{v0}"
//let v1 = v0.evolve r1.NextDouble r2.NextDouble
//printfn $"v1: %A{v1}"

let evolve i (v : StateVector) data =
    let vNew = v.evolve r1.NextDouble r2.NextDouble
//    printfn $"{i}, t: {vNew.time}, population: {vNew.state}"
    vNew, (getData i vNew) :: data

//steps
//|> List.fold (fun acc r -> evolve acc r) v0
//|> ignore


let getResults() =
    steps
    |> List.fold (fun (v, d) i -> evolve i v d) (v0, [])
    |> snd

let results, elapsed = time getResults ()

printfn $"Took: {elapsed}"

let foxData = results |> List.map (fun e -> e.t, double e.foxes)
let foxChart = Chart.Line(foxData, Name = "Foxes")

let hareData = results |> List.map (fun e -> e.t, double e.hares)
let hareChart = Chart.Line(hareData, Name = "Hares")

printfn "Plotting..."
foxChart |> Chart.show
hareChart |> Chart.show
