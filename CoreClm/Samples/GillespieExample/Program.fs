open System
open Gillespie.SsaSolver
open Plotly.NET

let fox = Any "fox"
let hare = Any "hare"

//let a = 1.0
//let b = 1.0
//let c = 1.0
//let d = 1.0
//let noOfFoxes = 20
//let noOfHares = 20

let a = 10.0
let b = 0.01
let c = 0.01
let d = 2.0
let noOfFoxes = 3
let noOfHares = 100000

// https://en.wikipedia.org/wiki/Lotka%E2%80%93Volterra_equations
let reactions =
    [
        // Hares multiply.
        // dx / dt = a * x
        {
            info = 
                {
                    input = [ hare, 1 ]
                    output = [ hare, 2 ]
                }
                |> normalized
            rate = ReactionRate a
        }
        
        // Hares got eaten.
        // dx / dt = -b * x * y
        {
            info = 
                {
                    input = [ (hare, 1); (fox, 1) ]
                    output = [ (fox, 1) ]
                }
                |> normalized
            rate = ReactionRate b
        }            
        
        // Foxes multiply.
        // dy / dt = d * x * y
        {
            info = 
                {
                    input = [ (fox, 1); (hare, 1) ]
                    output = [ (fox, 2); (hare, 1) ]
                }
                |> normalized
            rate = ReactionRate d
        }            
        
        // Foxes die off.
        // dy / dt = -c * x
        {
            info = 
                {
                    input = [ fox, 1 ]
                    output = [  ]
                }
                |> normalized
            rate = ReactionRate c
        }
    ]
    
    
let v0 =
    {
        state =
            [
                fox, NoOfMolecules noOfFoxes
                hare, NoOfMolecules noOfHares
            ]
            |> Map.ofList
        reactions = reactions
        volume = 1.0
        time = 0.0
    }
    
let r1 = Random(1)
let r2 = Random(2)


let steps = [ for i in 0..1000000 -> i ]

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
        foxes = (v.state |> Map.tryFind fox |> Option.defaultValue NoOfMolecules.defaultValue).value
        hares = (v.state |> Map.tryFind hare |> Option.defaultValue NoOfMolecules.defaultValue).value
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

let results =
    steps
    |> List.fold (fun (v, d) i -> evolve i v d) (v0, [])
    |> snd
    
let foxData = results |> List.map (fun e -> e.t, double e.foxes)
let foxChart = Chart.Line(foxData, Name = "Foxes")

let hareData = results |> List.map (fun e -> e.t, double e.hares)
let hareChart = Chart.Line(hareData, Name = "Hares")

printfn "Plotting..."
foxChart |> Chart.show
hareChart |> Chart.show

