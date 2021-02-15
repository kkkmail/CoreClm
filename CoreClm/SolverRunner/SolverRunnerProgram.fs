namespace SolverRunner

open ClmSys.ExitErrorCodes
open SolverRunner.SolverRunnerImplementation
open SolverRunner.SolverRunnerCommandLine
open Argu

module Program =

    [<EntryPoint>]
    let main argv =
//        match tryCreateSolver() with
//        | Ok solver ->
//            solver.run()
//            CompletedSuccessfully
//        | Error e ->
//            -1

    try
        let parser = ArgumentParser.Create<SolverRunnerArguments>(programName = SolverRunnerName)
        let results = parser.Parse argv
        let usage = parser.PrintUsage()
        runSolver results usage
    with
        | exn ->
            printfn $"{exn.Message}"
            UnknownException
