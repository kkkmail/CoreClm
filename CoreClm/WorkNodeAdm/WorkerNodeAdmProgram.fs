open Argu
open WorkerNodeAdm.AdmCommandLine
open WorkerNodeAdm.WorkerNodeAdmTasks
open ClmSys.ExitErrorCodes

[<EntryPoint>]
let main argv =
    try
        let parser = ArgumentParser.Create<WorkerNodeAdmArgs>(programName = WrkAdmAppName)
        let results = (parser.Parse argv).GetAllResults()

        match results |> WrkAdmTask.tryCreate with
        | Some task -> task.run()
        | None ->
            printfn "%s" (parser.PrintUsage())
            InvalidCommandLineArgs

    with
    | e ->
        printfn "%s" e.Message
        UnknownException
