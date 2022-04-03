open System
open Plotly.NET

open Softellect.Sys.Core
open Gillespie.SsaPrimitives

//open Gillespie.SsaSolver
open Gillespie.SsaSolverMutable

let noOfSteps = 1_000_000
//let noOfSteps = 1_000
let noOfFoxes = 10
let noOfHares = 1_000

let fox = Any "fox"
let hare = Any "hare"

//let a = 1.0
//let b = 1.0
//let c = 1.0
//let d = 1.0
//let noOfFoxes = 20
//let noOfHares = 20

let a = 1.0 // Hares multiply: dx / dt = a * x
let b = 0.03 // Hares got eaten: dx / dt = -b * x * y
let c = 1.0 // Foxes die off: dy / dt = -c * x
let d = 0.01 // Foxes multiply: dy / dt = d * x * y
let e = 0.1 // Hares resurrect: dx / dt = e
let f = 0.1 // Foxes resurrect: dx / dt = f
let g = 0.000_1 // Hares die from overpopulation: dx / dt = -g * x * x

// https://en.wikipedia.org/wiki/Lotka%E2%80%93Volterra_equations
let reactions =
    [
        {
            description = "Hares multiply: dx / dt = a * x"
            info =
                {
                    input = [ hare, 1 ]
                    output = [ hare, 2 ]
                }
                |> normalized
            rate = ReactionRate a
        }

        {
            description = "Hares got eaten: dx / dt = -b * x * y"
            info =
                {
                    input = [ (hare, 1); (fox, 1) ]
                    output = [ (fox, 1) ]
                }
                |> normalized
            rate = ReactionRate b
        }

        {
            description = "Hares resurrect: dx / dt = e"
            info =
                {
                    input = []
                    output = [ hare, 1 ]
                }
                |> normalized
            rate = ReactionRate e
        }

        {
            description = "Hares die from overpopulation: dx / dt = -g * x * x"
            info =
                {
                    input = [ hare, 2 ]
                    output = [ hare, 1 ]
                }
                |> normalized
            rate = ReactionRate g
        }

        {
            description = "Foxes multiply: dy / dt = d * x * y"
            info =
                {
                    input = [ (fox, 1); (hare, 1) ]
                    output = [ (fox, 2); (hare, 1) ]
                }
                |> normalized
            rate = ReactionRate d
        }

        {
            description = "Foxes die off: dy / dt = -c * x"
            info =
                {
                    input = [ fox, 1 ]
                    output = []
                }
                |> normalized
            rate = ReactionRate c
        }

        {
            description = "Foxes resurrect: dx / dt = f"
            info =
                {
                    input = []
                    output = [ fox, 1 ]
                }
                |> normalized
            rate = ReactionRate f
        }
    ]


let v0 =
    {
        state =
            [
                fox, NoOfMolecules noOfFoxes
                hare, NoOfMolecules noOfHares
            ]
            |> createSubstanceMap
        reactions = reactions
        volume = 1.0
        time = 0.0
    }

let r1 = Random(1)
let r2 = Random(2)


let steps = [ for i in 0..noOfSteps -> i ]

type LotkaVolterraData =
    {
        t : double
        foxes: int
        hares : int
        step: int
    }

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
