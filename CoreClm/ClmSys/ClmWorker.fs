namespace ClmSys

open Argu

module ClmWorker =
    let x = 1

    //[<CliPrefix(CliPrefix.None)>]
    //type WorkerArguments<'A when 'A :> IArgParserTemplate> =
    //    | [<Unique>] [<First>] [<AltCommandLine("r")>] Run of ParseResults<'A>
    //    | [<Unique>] [<First>] [<AltCommandLine("s")>] Save of ParseResults<'A>

    //    static member fromArgu c a : list<WorkerArguments<'A>> = a |> List.map (fun e -> c e)


    //type WorkerTask<'R, 'A when 'A :> IArgParserTemplate> =
    //    | RunServiceTask of (unit -> unit)
    //    | SaveSettingsTask of (unit -> unit)

    //    member task.run () =
    //        match task with
    //        | RunServiceTask r -> r()
    //        | SaveSettingsTask s -> s()

    //    static member private tryCreateRunServiceTask r (p : list<WorkerArguments<'A>>) : WorkerTask<'R, 'A> option =
    //        p |> List.tryPick (fun e -> match e with | Run p -> r p |> RunServiceTask |> Some | _ -> None)

    //    static member private tryCreateSaveSettingsTask s (p : list<WorkerArguments<'A>>) : WorkerTask<'R, 'A> option =
    //        p |> List.tryPick (fun e -> match e with | Save p -> s p |> SaveSettingsTask |> Some | _ -> None)

    //    static member tryCreate r s p : WorkerTask<'R, 'A> option =
    //        [
    //            WorkerTask.tryCreateRunServiceTask r
    //            WorkerTask.tryCreateSaveSettingsTask s
    //        ]
    //        |> List.tryPick (fun e -> e p)
