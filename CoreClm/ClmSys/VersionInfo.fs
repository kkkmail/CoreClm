namespace ClmSys

open Softellect.Sys.MessagingPrimitives

module VersionInfo =

    /// ! Do not forget to update messagingDataVersion in VersionInfo.ps1 when this parameter is updated !
    ///
    /// Increment BY TWO when:
    ///     1. Internal messaging structures change and messages can no longer be successfully transferred among components.
    ///     2. Some other updates were performed and we need to inform worker nodes that they need to upgrade.
    ///     3. Version number (below) is increased.
    let messagingDataVersion = MessagingDataVersion 110


    /// ! Do not forget to update versionNumber in VersionInfo.ps1 when this parameter is updated !
    ///
    /// This is an overall system version.
    [<Literal>]
    let VersionNumberValue = "6.0.1"


    /// A base name, which controls the database name and a working folder name.
    /// It is loosely the same as the version number.
    /// It must be updated when the old version is still running (for days) but the new version needs to be deployed.
    [<Literal>]
    let ClmBaseName = "clm601"


    [<Literal>]
    let MsgSvcBaseName = "msg601"


    [<Literal>]
    let WorkerNodeSvcBaseName = "wns601"


    /// Default port on which messaging communication is performed.
    let defaultServicePort = 5000 + messagingDataVersion.value


    /// Increment fractional part by 0.0001, e.g. 1.000 -> 1.001 if an updated version can read the previous version format.
    /// Increment integer part and reset fractional part, e.g. 1.023 -> 2.000, when the changes are completely incompatible with previous version.
    let FileStructureVersion = 5.000m


//    /// This is the name of the system. It is used, for example, to access Windows Registry.
//    [<Literal>]
//    let SystemName = "CLM"


    [<Literal>]
    let CopyrightInfo = "MIT License - Copyright Konstantin K. Konstantinov and Alisa F. Konstantinova © 2015 - 2020."


    type VersionNumber =
        | VersionNumber of string

        member this.value = let (VersionNumber v) = this in v


    let versionNumberValue = VersionNumber VersionNumberValue
