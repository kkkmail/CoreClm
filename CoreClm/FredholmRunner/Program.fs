open System.Diagnostics
open FredholmRunner.FredholmData

printfn "Starting..."
let sw = Stopwatch.StartNew()

let data =
    {
        noOfIntervals = 100
        l2 = 25
        epsEe = 0.05
        epsInf = 0.05
    }

// let kernel


printfn $"Elapsed: {sw.Elapsed.TotalSeconds}."
