namespace MessagingAdm

open System.Threading

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

open MessagingServiceInfo.ServiceInfo
open MessagingAdm.AdmCommandLine
open System

module MsgAdmTasks =

    let monitorService (service : MessagingService) =
    ////let monitorService (service : IMessagingWcfService) =
    //    let i = 30_000

    //    while true do
    //        try
    //            printfn "Getting messaging service state..."
    //            let state = service.getState()

    //            let n =
    //                match state with
    //                | Ok s -> s.msgInfo |> List.map (fun (_, e) -> e.Length) |> List.sum
    //                | Error _-> 0
    //            printfn "State at %A is: %A\nNumber of messages: %i.\n" DateTime.Now state n
    //        with
    //        | e -> printfn "Exception: %A\n" e.Message

    //        Thread.Sleep(i)

        ignore()


    let stopService (service : MessagingService) = () // service.configureService (MsgWorkState ShuttingDown)
    let startService (service : MessagingService) = () // service.configureService (MsgWorkState CanTransmitMessages)

    type MsgAdmTask =
        | MonitorMsgServiceTask of MessagingService
        | StartMsgServiceTask of MessagingService
        | StopMsgServiceTask of MessagingService


        member task.run () =
            match task with
            | MonitorMsgServiceTask s -> monitorService s
            | StartMsgServiceTask s -> startService s
            | StopMsgServiceTask s -> stopService s

        static member private tryCreateMonitorTask s p =
            p |> List.tryPick (fun e -> match e with | MonitorMsgService -> s |> MonitorMsgServiceTask |> Some | _ -> None)

        static member private tryCreatStopServiceTask s p =
            p |> List.tryPick (fun e -> match e with | StopMsgService -> s |> StopMsgServiceTask |> Some | _ -> None)

        static member private tryCreatStartServiceTask s p =
            p |> List.tryPick (fun e -> match e with | StartMsgService -> s |> StartMsgServiceTask |> Some | _ -> None)

        static member createTask s p =
            [
                MsgAdmTask.tryCreatStopServiceTask
                MsgAdmTask.tryCreatStartServiceTask
                MsgAdmTask.tryCreateMonitorTask
            ]
            |> List.tryPick (fun e -> e s p)
            |> Option.defaultWith (fun () -> MonitorMsgServiceTask s)
