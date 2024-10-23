namespace SolverRunner

//open ClmSys.ExitErrorCodes
open SolverRunner.SolverRunnerImplementation
open SolverRunner.SolverRunnerCommandLine
open ServiceProxy.SolverProcessProxy
open Argu
open Softellect.Sys.ExitErrorCodes

module Program =

    [<EntryPoint>]
    let main argv =
        try
            let parser = ArgumentParser.Create<SolverRunnerArguments>(programName = SolverRunnerName)
            let results = parser.Parse argv
            let usage = parser.PrintUsage()
            runSolverProcessImpl results usage
        with
        | exn ->
            printfn $"{exn.Message}"
            UnknownException
