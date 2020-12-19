namespace ClmSys

open System

open Softellect.Sys.Core

open ClmSys.VersionInfo
open GeneralPrimitives
open ContGenPrimitives


module GeneralData =

    [<Literal>]
    let DefaultRootDrive = "C"


    /// String.Empty is not a const.
    [<Literal>]
    let EmptyString = ""


    /// Environment.NewLine is too long and it is not a const.
    [<Literal>]
    let Nl = "\r\n"


    let appSettingsFile = "appsettings.json"


    let getVersionImpl getter p =
        match getter p with
        | Some x -> x
        | None -> versionNumberValue


    let toValidServiceName (serviceName : string) =
        serviceName.Replace(" ", "").Replace("-", "").Replace(".", "")


    //let getServiceUrlImpl (ServiceAddress serviceAddress) (ServicePort servicePort) serviceName =
    //    "tcp://" + serviceAddress + ":" + (servicePort.ToString()) + "/" + serviceName


    //let getWcfServiceUrlImpl (ServiceAddress serviceAddress) (ServicePort servicePort) serviceName =
    //    "net.tcp://" + serviceAddress + ":" + (servicePort.ToString()) + "/" + serviceName


    type ResultDataId
        with
        member this.toRunQueueId() = this.value |> RunQueueId


    let estimateEndTime progress (started : DateTime) =
        if progress > 0.0m && progress <= 1.0m
        then
            let estRunTime = (decimal (DateTime.Now.Subtract(started).Ticks)) / progress |> int64 |> TimeSpan.FromTicks
            started.Add estRunTime |> Some
        else None


    type TaskProgress
        with

        member progress.estimateEndTime (started : DateTime) =
            match progress with
            | NotStarted -> None
            | InProgress p -> estimateEndTime p started
            | Completed _ -> Some DateTime.Now
            | Failed _ -> None
            | Cancelled _ -> None
            | AllCoresBusy _ -> None


    let timedImpl l n f = timedImplementation true l n f
//    let timed name f a = timedImpl logger name (fun () -> f a)
