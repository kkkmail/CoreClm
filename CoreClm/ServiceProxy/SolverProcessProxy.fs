namespace ServiceProxy

open System
open System.Diagnostics
open ClmSys.GeneralPrimitives
open ClmSys.SolverRunnerPrimitives
open ClmSys.ClmErrors
open MessagingServiceInfo.ServiceInfo
open DbData.WorkerNodeDatabaseTypes
open DbData.Configuration

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


    type SolverProcessProxy =
        {
            tryLoadRunQueue : unit -> ClmResult<WorkerNodeRunModelData>
            tryStartRunQueue : unit -> UnitResult
            tryCompleteRunQueue : unit -> UnitResult
            tryCancelRunQueue : string -> UnitResult
            tryFailRunQueue : string -> UnitResult
        }


        static member create (q : RunQueueId) : SolverProcessProxy =
            let c = getWorkerNodeSvcConnectionString
            let p = Process.GetCurrentProcess().Id |> ProcessId

            {
                tryLoadRunQueue = fun () -> tryLoadRunQueue c q
                tryStartRunQueue = fun () -> tryStartRunQueue c q p
                tryCompleteRunQueue = fun () -> tryCompleteRunQueue c q
                tryCancelRunQueue = tryCancelRunQueue c q
                tryFailRunQueue = tryFailRunQueue c q
            }
