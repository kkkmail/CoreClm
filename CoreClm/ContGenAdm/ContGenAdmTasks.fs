namespace ContGenAdm

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

module ContGenAdmTasks =

    let logError e =
        printfn "Error occurred: %A" e
        Ok()


    let loadClmDefaultValue = loadClmDefaultValue getClmConnectionString


    let addClmTask (p :list<AddClmTaskArgs>) =
        let i = tryGetClmDefaultValueId p
        let n = tryGetNumberOfAminoAcids p
        let m = tryGetMaxPeptideLength p
        let c = tryGetCommandLineParams p

        match i, n, m, c with
        | Some i, Some n, Some m, Some c ->
            printfn "Updating parameters. Using number of amino acids: %A, max peptide length: %A, index of default: %A." (n.length) (m.length) i
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

                match addClmTask getClmConnectionString t with
                | Ok() ->
                    match getGenerateModelCode p with
                    | true ->
                        printfn "Genetrating model..."
                        let proxy = GenerateModelProxy.create getClmConnectionString

                        match generateModel proxy t with
                        | Ok model ->
                            match generateModelCode model t with
                            | Ok _ -> Ok()
                            | Error e -> logError e
                        | Error e -> logError e
                    | false -> Ok()
                | Error e -> logError e
            | Error e ->
                printfn "updateParameters: Cannot find data for default set index %A, Error: %A" i e
                Ok()
        | _ ->
            printfn "updateParameters: Incorrect number of amino acids and/or max peptide length and/or index of default specified."
            Ok()


    let monitor (p :list<MonitorArgs>) =
        let i =
            match p |> List.tryPick (fun e -> match e with | RefreshInterval i -> Some i) with
            | Some i -> i * 1_000
            | None -> 30_000

        let modelMonitor = ModelMonitor.create getClmConnectionString

        while true do
            try
                getServiceState modelMonitor.getRunState |> ignore
            with
            | e -> printfn "Exception: %A\n" e.Message

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
