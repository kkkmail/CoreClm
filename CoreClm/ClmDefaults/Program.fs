open ClmSys.ExitErrorCodes
open ClmDefaults.ClmDefaultsTasks
open Argu

[<EntryPoint>]
let main argv =
    try
        saveAllDefaults() |> ignore
        CompletedSuccessfully
    with
        | exn ->
            printfn "%s" exn.Message
            UnknownException

    //try
    //    let parser = ArgumentParser.Create<ContGenArguments>(programName = ContGenAppName)
    //    let results = parser.Parse argv

    //    match results.GetAllResults() |> ContGenTask.tryCreate with
    //    | Some task -> task.run()
    //    | None ->
    //        printfn "%s" (parser.PrintUsage())
    //        InvalidCommandLineArgs
    //with
    //    | exn ->
    //        printfn "%s" exn.Message
    //        UnknownException
