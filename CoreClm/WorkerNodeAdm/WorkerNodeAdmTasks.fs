﻿namespace WorkerNodeAdm

open Softellect.Wcf.Common

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
            let service = WorkerNodeResponseHandler(i.workerNodeServiceAccessInfo, NetTcpCommunication, WcfSecurityMode.defaultValue)

            while true do
                try
                    getServiceState service DummyWrkMonitorParam
                with
                | e -> printfn $"Exception: %A{e.Message}\n"

                Thread.Sleep(d)
        | Error e -> printfn $"Error occurred: %A{e}"

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
