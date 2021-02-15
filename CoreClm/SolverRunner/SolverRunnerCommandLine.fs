namespace SolverRunner

open Argu
open System
open ClmSys.GeneralPrimitives

module SolverRunnerCommandLine =

    [<Literal>]
    let SolverRunnerName = "SolverRunner.exe"


    [<CliPrefix(CliPrefix.None)>]
    type SolverRunnerArguments =
        | [<Unique>] [<AltCommandLine("q")>]  RunQueue of Guid

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | RunQueue _ -> "specify RunQueueId."
