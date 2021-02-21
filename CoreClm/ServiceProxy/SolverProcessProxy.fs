namespace ServiceProxy

open System
open System.Diagnostics
open ClmSys.GeneralPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.SolverRunnerPrimitives
open ClmSys.ContGenPrimitives
open ClmSys.SolverRunnerErrors
open ClmSys.ClmErrors
open ClmSys.VersionInfo
open MessagingServiceInfo.ServiceInfo
open DbData.WorkerNodeDatabaseTypes
open DbData.MsgSvcDatabaseTypes
open Softellect.Messaging.Client

module SolverProcessProxy =

    [<Literal>]
    let SolverRunnerName = "SolverRunner.exe"


    let getAssemblyLocation() =
        let x = Uri(System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)).LocalPath
        x

    let getExeName exeName =
        let location = getAssemblyLocation()
        location + @"\" + exeName


    let runSolverProcess (RunQueueId q) =
        let fileName = SolverRunnerName
        let args = $"q {q}"

        try
            let procStartInfo =
                ProcessStartInfo(
                    RedirectStandardOutput = false,
                    RedirectStandardError = false,
                    UseShellExecute = false,
                    FileName = getExeName fileName,
                    Arguments = args
                )

            procStartInfo.WorkingDirectory <- getAssemblyLocation()
            let p = new Process(StartInfo = procStartInfo)
            let started = p.Start()

            if started
            then
                p.PriorityClass <- ProcessPriorityClass.Idle
                let processId = p.Id |> ProcessId
                printfn $"Started: {p.ProcessName} with pid: {processId}."
                Some processId
            else
                printfn $"Failed to start process: {fileName}."
                None
        with
        | ex ->
            printfn $"Failed to start process: {fileName} with exception: {ex}."
            None


    /// Checks that
    /// Returns Ok when a given RunQueueId is NOT running and when a number of
    /// running solvers is less than a maximum allowed number.
    let checkRunning (RunQueueId q) n : CheckRunningResult =
        try
            let v = $"{q}".ToLower()

            let processes =
                Process.GetProcessesByName(SolverRunnerName)
                |> Array.map (fun e ->  e.Id, e.StartInfo.Arguments)
                |> Array.map (fun (i, e) -> i, e.ToLower())

            match processes.Length < n with
            | true ->
                let p =
                    processes
                    |> Array.map (fun (i, e) -> i, e.Contains(v))
                    |> Array.tryFind snd

                match p with
                | None -> CanRun
                | Some (i, _) -> i |> ProcessId |> AlreadyRunning
            | false -> TooManyRunning processes.Length
        with
        | e -> e |> GetProcessesByNameExn


    type SendMessageProxy =
        {
            partitionerId : PartitionerId
            sendMessage : MessageInfo -> UnitResult
        }


    type OnUpdateProgressProxy =
        {
            tryDeleteWorkerNodeRunModelData : unit -> UnitResult
            tryUpdateProgress : TaskProgress -> UnitResult
            sendMessageProxy : SendMessageProxy
        }


//    type SolverProcessProxy =
//        {
//            tryLoadRunQueue : unit -> ClmResult<WorkerNodeRunModelData>
//            tryStartRunQueue : unit -> UnitResult
//            tryCompleteRunQueue : unit -> UnitResult
//            tryCancelRunQueue : string -> UnitResult
//            tryFailRunQueue : string -> UnitResult
//            checkRunning : unit -> UnitResult
//            onUpdateProgressProxy : OnUpdateProgressProxy
//        }
//
//
//        static member create c m p q : SolverProcessProxy =
//            let pid = Process.GetCurrentProcess().Id |> ProcessId
//
//            // Send the message directly to local database.
//            let sendMessage i =
//                createMessage messagingDataVersion m i
//                |> saveMessage c
//
//            {
//                tryLoadRunQueue = fun () -> tryLoadRunQueue c q
//                tryStartRunQueue = fun () -> tryStartRunQueue c q pid
//                tryCompleteRunQueue = fun () -> tryCompleteRunQueue c q
//                tryCancelRunQueue = tryCancelRunQueue c q
//                tryFailRunQueue = tryFailRunQueue c q
//                checkRunning = fun () -> checkRunning q
//
//                onUpdateProgressProxy =
//                    {
//                        tryDeleteWorkerNodeRunModelData = fun () -> deleteRunQueue c q
//                        tryUpdateProgress = tryUpdateProgressRunQueue c q
//
//                        sendMessageProxy =
//                            {
//                                partitionerId = p
//                                sendMessage = sendMessage
//                            }
//                    }
//            }
