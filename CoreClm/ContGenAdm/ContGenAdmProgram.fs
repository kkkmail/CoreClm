﻿open Argu
open ContGenAdm.AdmCommandLine
open ContGenAdm.ContGenAdmTasks
//open ClmSys.ExitErrorCodes
//open ClmSys.Logging
open Softellect.Sys.ExitErrorCodes

[<EntryPoint>]
let main argv =
    try
        let parser = ArgumentParser.Create<ContGenAdmArguments>(programName = ContGenAdmAppName)
        let results = (parser.Parse argv).GetAllResults()

        match results |> ContGenAdmTask.tryCreate with
        | Some task ->
            task.run() |> ignore
            CompletedSuccessfully
        | None ->
            printfn $"%s{parser.PrintUsage()}"
            InvalidCommandLineArgs
    with
    | exn ->
        printfn $"%s{exn.Message}"
        UnknownException
