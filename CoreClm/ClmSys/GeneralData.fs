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


    type ResultDataId
        with
        member this.toRunQueueId() = this.value |> RunQueueId


//    type TaskProgress
//        with
//
//        member progress.estimateEndTime (started : DateTime) =
//            match progress with
//            | NotStarted -> None
//            | InProgress p -> estimateEndTime (decimal p.progressDetailed) started
//            | Completed _ -> Some DateTime.Now
//            | Failed _ -> None
//            | Cancelled _ -> None
//            | AllCoresBusy _ -> None


    let timedImpl l n f = timedImplementation true l n f
//    let timed name f a = timedImpl logger name (fun () -> f a)
