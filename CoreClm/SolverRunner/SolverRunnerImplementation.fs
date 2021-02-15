namespace SolverRunner

open Argu
open ClmSys.ClmErrors
open ClmSys.ExitErrorCodes
open ClmSys.GeneralPrimitives
open ServiceProxy.SolverProcessProxy
open SolverRunner.SolverRunnerCommandLine

module SolverRunnerImplementation =

    type SolverProcess(proxy : SolverProcessProxy) =

        member x.run() : unit =
            failwith "SolverProcess.run is not yet implemented."


    let tryCreateSolver q : ClmResult<SolverProcess> =
        let proxy = SolverProcessProxy.create q
        failwith "tryCreateSolver is not yet implemented."


    let runSolver (results : ParseResults<SolverRunnerArguments>) usage : int =
        match results.TryGetResult RunQueue with
        | Some q ->
            match RunQueueId q |> tryCreateSolver with
            | Ok solver ->
                solver.run()
                CompletedSuccessfully
            | Error e ->
                printfn $"runSolver: Error: {e}."
                DatabaseErrorOccurred
        | None ->
            printfn $"runSolver: {usage}."
            InvalidCommandLineArgs

