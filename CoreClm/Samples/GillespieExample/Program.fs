open System
open Gillespie.SsaSolver

printfn "Hello from F#"

let fox = Any "fox"
let hare = Any "hare"

//let a = 1.0
//let b = 1.0
//let c = 1.0
//let d = 1.0
//let noOfFoxes = 20
//let noOfHares = 20

let a = 0.1
let b = 0.1
let c = 0.1
let d = 0.1
let noOfFoxes = 2
let noOfHares = 1000

// https://en.wikipedia.org/wiki/Lotka%E2%80%93Volterra_equations
let reactions =
    [
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


let steps = [ for i in 0..1000 -> i ]
let evolution =
    steps
    
printfn $"v0: %A{v0}"
let v1 = v0.evolve r1.NextDouble r2.NextDouble
printfn $"v1: %A{v1}"

let evolve (v : StateVector) i =
    let vNew = v.evolve r1.NextDouble r2.NextDouble
    printfn $"{i}, t: {vNew.time}, population: {vNew.state}"
    vNew

steps
|> List.fold (fun acc r -> evolve acc r) v0
|> ignore
