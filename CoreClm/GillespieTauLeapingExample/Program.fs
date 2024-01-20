open System
open Plotly.NET

open Softellect.Sys.Core
open Gillespie.TauLeaping2
open Gillespie.LotkaVolterra


let hareIndex = 0
let foxIndex = 1


let createLotkaVolterraSystem (rates: LotkaVolterraRateData) =
    {
        molecules = [| 1_000; 10 |]  // Initial counts: 1_000 hares, 10 foxes
        reactions =
            [
                { reactants = [{ index = hareIndex; count = 1 }]; products = [{ index = hareIndex; count = 2 }]; rateConstant = rates.haresMultiplyRate }
                { reactants = [{ index = hareIndex; count = 1 }; { index = foxIndex; count = 1 }]; products = [{ index = foxIndex; count = 1 }]; rateConstant = rates.haresEatenRate }
                { reactants = []; products = [{ index = hareIndex; count = 1 }]; rateConstant = rates.haresResurrectRate }
                { reactants = [{ index = hareIndex; count = 2 }]; products = [{ index = hareIndex; count = 1 }]; rateConstant = rates.haresDieOffRate }
                { reactants = [{ index = foxIndex; count = 1 }]; products = [{ index = foxIndex; count = 2 }]; rateConstant = rates.foxesMultiplyRate }
                { reactants = [{ index = foxIndex; count = 1 }]; products = []; rateConstant = rates.foxesDieOffRate }
                { reactants = []; products = [{ index = foxIndex; count = 1 }]; rateConstant = rates.foxesResurrectRate }
            ]
    }


// Define the rates for the Lotka-Volterra model
let rates =
    {
        haresMultiplyRate = 1.0
        haresEatenRate = 0.03
        foxesDieOffRate = 1.0
        foxesMultiplyRate = 0.01
        haresResurrectRate = 0.1
        foxesResurrectRate = 0.1
        haresDieOffRate = 0.000_1
    }

// Create the initial system state
let initialState = createLotkaVolterraSystem rates

let tau = 10.0
let numSteps = 1_000_000
//let numSteps = 100

let getResults() = simulateTauLeaping initialState tau numSteps

let results, elapsed = time getResults ()
printfn $"Took: {elapsed}"


let foxData =
    results
    |> List.mapi (fun i state -> (float i) * tau, float state.molecules.[foxIndex])

let hareData =
    results
    |> List.mapi (fun i state -> (float i) * tau, float state.molecules.[hareIndex])

let foxChart = Chart.Line(foxData, Name = "Foxes")
let hareChart = Chart.Line(hareData, Name = "Hares")

printfn "Plotting..."
foxChart |> Chart.show
hareChart |> Chart.show
