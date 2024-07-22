namespace Primitives

open Softellect.Sys.Core
open Primitives.VersionInfo

module GeneralData =

    ///// String.Empty is not a const.
    //[<Literal>]
    //let EmptyString = ""


    ///// Environment.NewLine is too long and it is not a const.
    //[<Literal>]
    //let Nl = "\r\n"


    //[<Literal>]
    //let DefaultRootDrive = "C"


    //let appSettingsFile = "appsettings.json"


    let getVersionImpl getter p =
        match getter p with
        | Some x -> x
        | None -> versionNumberValue


    //let toValidServiceName (serviceName : string) =
    //    serviceName.Replace(" ", "").Replace("-", "").Replace(".", "")


    let timedImpl l n f = timedImplementation true l n f
