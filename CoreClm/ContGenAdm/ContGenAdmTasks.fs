namespace ContGenAdm

open System.Diagnostics
open System.Threading
open ContGenServiceInfo.ServiceInfo
open DbData.Configuration
open DbData.DatabaseTypes
open Clm.ModelParams
open System
open ContGenAdm.AdmCommandLine
open ClmSys.ContGenPrimitives
open ContGen.ModelRunner
open ContGen.ModelGenerator
open ServiceProxy.ModelGeneratorProxy
open ClmImpure

module ContGenAdmTasks =

    let logError e =
        printfn $"Error occurred: %A{e}"
        Ok()


    let loadClmDefaultValue = loadClmDefaultValue getContGenConnectionString


    let addClmTask (p :list<AddClmTaskArgs>) =
        let i = tryGetClmDefaultValueId p
        let n = tryGetNumberOfAminoAcids p
        let m = tryGetMaxPeptideLength p
        let c = tryGetCommandLineParams p
        let so = getSeedValue p

        let settings = loadContGenSettings()
//        printfn "addClmTask: settings =\n%A" settings

        let dictionaryUpdateType = getDictionaryUpdateType p |> Option.defaultValue settings.contGenInfo.dictionaryUpdateType
//        printfn $"addClmTask: dictionaryUpdateType = {dictionaryUpdateType}"

        match i, n, m, c with
        | Some i, Some n, Some m, Some c ->
            printfn $"addClmTask: Using number of amino acids: {n.length}, max peptide length: {m.length}, index of default: {i}."
            match loadClmDefaultValue i with
            | Ok _ ->
                let r = getNumberOrRepetitions p

                let t =
                    {
                        clmTaskInfo =
                            {
                                clmTaskId = ClmTaskId.getNewId()
                                clmDefaultValueId = i
                                numberOfAminoAcids = n
                                maxPeptideLength = m
                            }
                        commandLineParams = c
                        numberOfRepetitions = r
                        remainingRepetitions = r
                        createdOn = DateTime.Now
                    }

                match addClmTask getContGenConnectionString t with
                | Ok() ->
                    match getGenerateModelCode p with
                    | true ->
                        let sw = Stopwatch()
                        sw.Start()
//                        printfn "addClmTask: Generating model..."
                        let proxy = GenerateModelProxy.create dictionaryUpdateType settings.contGenInfo.collisionData so getContGenConnectionString
                        let fno = getModelCodeFileName p

                        let result =
                            match generateModel proxy t with
                            | Ok model ->
                                match generateModelCode model t fno with
                                | Ok _ -> Ok()
                                | Error e -> logError e
                            | Error e -> logError e

                        let elapsed = String.Format("{0:hh\\:mm\\:ss}", sw.Elapsed)
                        use proc = Process.GetCurrentProcess()
                        let memUsed = Math.Round((double proc.PeakWorkingSet64) / (1024.0 * 1024.0 * 1024.0), 2)
                        printfn $"addClmTask: Total generation time: {elapsed}, max memory used: %.2f{memUsed} GiB.\n"
                        result
                    | false -> Ok()
                | Error e -> logError e
            | Error e ->
                printfn $"addClmTask: Cannot find data for default set index %A{i}, Error: %A{e}"
                Ok()
        | _ ->
            printfn "addClmTask: Incorrect number of amino acids and/or max peptide length and/or index of default specified."
            Ok()


    let monitor (p :list<MonitorArgs>) =
        let i =
            match p |> List.tryPick (fun e -> match e with | RefreshInterval i -> Some i) with
            | Some i -> i * 1_000
            | None -> 30_000

        let modelMonitor = ModelMonitor.create getContGenConnectionString

        while true do
            try
                getServiceState modelMonitor.getRunState |> ignore
            with
            | e -> printfn $"Exception: %A{e.Message}\n"

            Thread.Sleep(i)
        Ok()


    type ContGenAdmTask =
        | AddClmTaskTask of list<AddClmTaskArgs>
        | MonitorTask of list<MonitorArgs>
        | ModifyRunQueueTask of list<ModifyRunQueueArgs>

        member task.run logger =
            match task with
            | AddClmTaskTask p -> addClmTask p
            | MonitorTask p -> monitor p
            | ModifyRunQueueTask p -> tryModifyRunQueueImpl logger p

        static member private tryCreateUpdateParametersTask p =
            p |> List.tryPick (fun e -> match e with | AddClmTask q -> q.GetAllResults() |> AddClmTaskTask |> Some | _ -> None)

        static member private tryCreateMonitorTask p =
            p |> List.tryPick (fun e -> match e with | Monitor q -> q.GetAllResults() |> MonitorTask |> Some | _ -> None)

        static member private tryCreatModifyRunQueueTask p =
            p |> List.tryPick (fun e -> match e with | ModifyRunQueue q -> q.GetAllResults() |> ModifyRunQueueTask |> Some | _ -> None)

        static member tryCreate p =
            [
                ContGenAdmTask.tryCreateUpdateParametersTask
                ContGenAdmTask.tryCreatModifyRunQueueTask
                ContGenAdmTask.tryCreateMonitorTask
            ]
            |> List.tryPick (fun e -> e p)
