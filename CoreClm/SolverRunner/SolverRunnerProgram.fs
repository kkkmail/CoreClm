namespace SolverRunner

//open ClmSys.ExitErrorCodes
//open SolverRunner.SolverRunnerImplementation
//open SolverRunner.SolverRunnerCommandLine
//open ServiceProxy.SolverProcessProxy
open Argu
open Softellect.Sys.ExitErrorCodes
open System
open System.IO
open Softellect.Sys.ExitErrorCodes
open Softellect.DistributedProcessing.SolverRunner.Program
open Softellect.DistributedProcessing.Primitives.Common
//open Softellect.Samples.DistrProc.Core.Primitives
open Softellect.DistributedProcessing.SolverRunner.Primitives
open Softellect.DistributedProcessing.SolverRunner.OdeSolver
open Softellect.Sys.Primitives
open Softellect.Sys.Core
//open Wolfram.NETLink
open Plotly.NET
open Giraffe.ViewEngine
open Softellect.Analytics.Wolfram
open Clm.ClmData
open ClmSys.SolverRunnerPrimitives


module Program =

    //[<EntryPoint>]
    //let main argv =
    //    try
    //        let parser = ArgumentParser.Create<SolverRunnerArguments>(programName = SolverRunnerName)
    //        let results = parser.Parse argv
    //        let usage = parser.PrintUsage()
    //        runSolverProcessImpl results usage
    //    with
    //    | exn ->
    //        printfn $"{exn.Message}"
    //        UnknownException


    [<EntryPoint>]
    let main argv =
        let retVal =
            try
                let chartGenerator =
                    {
                        getResultData = fun _ t (x : double[]) -> failwith "getResultData is not implemented yet."
                        generateResults = fun q d _ c -> None
                        generateDetailedResults = fun _ _ _ _ -> None
                    }

                let getUserProxy (solverData : ClmSolverData) =
                    let solverRunner = createOdeSolver solverData.inputParams solverData.odeParams

                    let solverProxy =
                        {
                            getInitialData = _.initialValues
                            getProgressData = None
                            getInvariant = fun _ _ _ -> RelativeInvariant 1.0
                            getOptionalFolder = fun _ _ -> None
                        }

                    {
                        solverRunner = solverRunner
                        solverProxy = solverProxy
                        resultGenerator = chartGenerator
                    }

                // Call solverRunnerMain<'D, 'P, 'X, 'C>
                printfn "Calling solverRunnerMain..."
                solverRunnerMain<ClmSolverData, ClmProgressData, double[], ClmChartData> clmSolverId getUserProxy argv
            with
            | e ->
                Console.WriteLine($"Exception: %A{e}.")
                CriticalError

        Console.ReadLine() |> ignore
        retVal
