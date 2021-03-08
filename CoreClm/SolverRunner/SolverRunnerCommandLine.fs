namespace SolverRunner

open Argu
open System


module SolverRunnerCommandLine =

    [<CliPrefix(CliPrefix.None)>]
    type SolverRunnerArguments =
        | [<Mandatory>] [<Unique>] [<AltCommandLine("q")>] RunQueue of Guid

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | RunQueue _ -> "specify RunQueueId."
