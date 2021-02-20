namespace ServiceProxy

open System
open System.Diagnostics
open ClmSys.GeneralPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.SolverRunnerPrimitives
open ClmSys.ClmErrors
open ClmSys.VersionInfo
open MessagingServiceInfo.ServiceInfo
open DbData.WorkerNodeDatabaseTypes
open DbData.Configuration
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


    let checkRunning (RunQueueId q) : UnitResult =
        failwith "checkRunning is not yet implemented."


    type SendMessageProxy =
        {
            partitionerId : PartitionerId
            sendMessage : MessageInfo -> UnitResult
        }


    type OnUpdateProgressProxy =
        {
            tryDeleteWorkerNodeRunModelData : RunQueueId -> UnitResult
            sendMessageProxy : SendMessageProxy
        }


    type SolverProcessProxy =
        {
            tryLoadRunQueue : unit -> ClmResult<WorkerNodeRunModelData>
            tryStartRunQueue : unit -> UnitResult
            tryCompleteRunQueue : unit -> UnitResult
            tryCancelRunQueue : string -> UnitResult
            tryFailRunQueue : string -> UnitResult
            checkRunning : unit -> UnitResult
            onUpdateProgressProxy : OnUpdateProgressProxy
        }


        static member create c m p q : SolverProcessProxy =
            let pid = Process.GetCurrentProcess().Id |> ProcessId

            // Send the message directly to local database.
            let sendMessage i =
                createMessage messagingDataVersion m i
                |> saveMessage c

            {
                tryLoadRunQueue = fun () -> tryLoadRunQueue c q
                tryStartRunQueue = fun () -> tryStartRunQueue c q pid
                tryCompleteRunQueue = fun () -> tryCompleteRunQueue c q
                tryCancelRunQueue = tryCancelRunQueue c q
                tryFailRunQueue = tryFailRunQueue c q
                checkRunning = fun () -> checkRunning q

                onUpdateProgressProxy =
                    {
                        tryDeleteWorkerNodeRunModelData = deleteRunQueue c

                        sendMessageProxy =
                            {
                                partitionerId = p
                                sendMessage = sendMessage
                            }
                    }
            }
