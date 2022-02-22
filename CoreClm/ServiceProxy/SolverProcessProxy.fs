namespace ServiceProxy

open System
open System.Diagnostics
open System.Management
open ClmSys.GeneralPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.SolverRunnerPrimitives
open ClmSys.SolverRunnerErrors
open ClmSys.ClmErrors
open MessagingServiceInfo.ServiceInfo

module SolverProcessProxy =

    [<Literal>]
    let SolverRunnerName = "SolverRunner.exe"


    [<Literal>]
    let SolverRunnerProcessName = "SolverRunner"


    let getAssemblyLocation() =
        let x = Uri(System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)).LocalPath
        x

    let getExeName exeName =
        let location = getAssemblyLocation()
        location + @"\" + exeName


    /// http://codebetter.com/matthewpodwysocki/2010/02/05/using-and-abusing-the-f-dynamic-lookup-operator/
    let (?) (this : 'Source) (prop : string) : 'Result =
        let t = this.GetType()
        let p = t.GetProperty(prop)
        p.GetValue(this, null) :?> 'Result


    /// Returns CanRun when a given RunQueueId is NOT used by any of the running solvers
    /// except the current one and when a number of running solvers is less than a maximum allowed value.
    ///
    /// See:
    ///     https://stackoverflow.com/questions/504208/how-to-read-command-line-arguments-of-another-process-in-c
    ///     https://docs.microsoft.com/en-us/dotnet/core/porting/windows-compat-pack
    ///     https://stackoverflow.com/questions/33635852/how-do-i-convert-a-weakly-typed-icollection-into-an-f-list
    let checkRunning no (RunQueueId q) : CheckRunningResult =
        try
            let v = $"{q}".ToLower()
            let pid = Process.GetCurrentProcess().Id

            let wmiQuery = $"select Handle, CommandLine from Win32_Process where Caption = '{SolverRunnerName}'"
            let searcher = new ManagementObjectSearcher(wmiQuery)
            let retObjectCollection = searcher.Get()

            let processes =
                retObjectCollection
                |> Seq.cast
                |> List.ofSeq
                |> List.map (fun e -> e :> ManagementObject)
                |> List.map (fun e -> e.["Handle"], e.["CommandLine"])
                |> List.map (fun (a, b) -> int $"{a}", $"{b}")

            let run() =
                let p =
                    processes
                    |> List.map (fun (i, e) -> i, e.ToLower().Contains(v) && i <> pid)
                    |> List.tryFind snd

                match p with
                | None -> CanRun
                | Some (i, _) -> i |> ProcessId |> AlreadyRunning

            match no with
            | Some n ->
                match processes.Length <= n with
                | true -> run()
                | false -> TooManyRunning processes.Length
            | None -> run()
        with
        | e -> e |> GetProcessesByNameExn


    /// Tries to run a solver with a given RunQueueId if it not already running and if the number
    /// of running solvers is less than a given allowed max value.
    let tryRunSolverProcess n (RunQueueId q) =
        let fileName = SolverRunnerName

        let run() =
            // TODO kk:20210511 - Build command line using Argu.
            let args = $"q {q}"

            try
                let procStartInfo =
                    ProcessStartInfo(
                        RedirectStandardOutput = false,
                        RedirectStandardError = false,
                        UseShellExecute = true,
                        FileName = getExeName fileName,
                        Arguments = args
                    )

                procStartInfo.WorkingDirectory <- getAssemblyLocation()
                procStartInfo.WindowStyle <- ProcessWindowStyle.Hidden
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

        // Decrease max value by one to account for the solver to be started.
        match checkRunning (Some (n - 1)) (RunQueueId q) with
        | CanRun -> run()
        | e ->
            printfn $"Can't run run queue with id {q}: %A{e}."
            None


    type SendMessageProxy =
        {
            partitionerId : PartitionerId
            sendMessage : MessageInfo -> UnitResult
        }


    type OnUpdateProgressProxy =
        {
            tryDeleteWorkerNodeRunModelData : unit -> UnitResult
            tryUpdateProgressData : ProgressData -> UnitResult
            sendMessageProxy : SendMessageProxy
        }
