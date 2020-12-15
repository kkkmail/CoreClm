namespace WorkerNodeAdm

open Softellect.Sys.Logging
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.ServiceInstaller
open Softellect.Sys.Primitives
open Softellect.Sys.Core
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.MessagingServiceErrors
open Softellect.Messaging.ServiceInfo
open Softellect.Sys.Core
open Softellect.Sys.Primitives
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.Logging
open Softellect.Sys.MessagingErrors
open Softellect.Wcf.Common
open Softellect.Wcf.Service
open Softellect.Messaging.Primitives
open Softellect.Messaging.ServiceInfo
open Softellect.Messaging.Service
open Softellect.Messaging.Client
open Softellect.Messaging.Proxy
open Softellect.Sys.MessagingClientErrors
open Softellect.Sys.MessagingServiceErrors

open WorkerNodeAdm.AdmCommandLine
open WorkerNodeServiceInfo.ServiceInfo
open ClmSys.WorkerNodeData
open System.Threading
open ClmSys.ExitErrorCodes

module WorkerNodeAdmTasks =

    let monitor p =
        let d = 30_000
        match getServiceAccessInfo p with
        | Ok i ->
            let service = WorkerNodeResponseHandler(i.workerNodeServiceAccessInfo, NetTcpCommunication)

            while true do
                try
                    getServiceState service DummyWrkMonitorParam
                with
                | e -> printfn "Exception: %A\n" e.Message

                Thread.Sleep(d)
        | Error e -> printfn "Error occurred: %A" e

        CompletedSuccessfully


    type  WrkAdmTask =
        //| ConfigureWorkerNodeTask of IWorkerNodeService * list<WorkerNodeConfigParam>
        | MonitorWorkerNodeTask of list<WorkerNodeAdmArgs>

        member task.run () =
            match task with
            //| ConfigureWorkerNodeTask (s, p) -> p |> List.map s.configure |> ignore
            | MonitorWorkerNodeTask p -> monitor p

        //static member private tryCreateConfigureTask s (i : WorkerNodeServiceInfo) (p : list<WorkerNodeAdmArgs>) =
        //    p |> List.tryPick (fun e -> match e with | ConfigureWrkService -> (s, [ WorkerNumberOfSores i.workerNodeInfo.noOfCores ]) |> ConfigureWorkerNodeTask |> Some | _ -> None)

        static member private tryCreateMonitorTask (p : list<WorkerNodeAdmArgs>) =
            p |> List.tryPick (fun e -> match e with | MonitorWrkService -> p |> MonitorWorkerNodeTask |> Some | _ -> None)

        static member tryCreate (p : list<WorkerNodeAdmArgs>) =
            [
                    WrkAdmTask.tryCreateMonitorTask
                    //WrkAdmTask.tryCreateConfigureTask
            ]
            |> List.tryPick (fun e -> e p)
