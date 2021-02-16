namespace ServiceProxy

open System.Diagnostics
open ClmSys.GeneralPrimitives
open ClmSys.SolverRunnerPrimitives
open ClmSys.ClmErrors
open MessagingServiceInfo.ServiceInfo
open DbData.WorkerNodeDatabaseTypes
open DbData.Configuration

module SolverProcessProxy =

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
