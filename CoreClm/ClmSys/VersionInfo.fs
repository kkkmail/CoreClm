namespace ClmSys

open Softellect.Sys.MessagingPrimitives

module VersionInfo =

    /// ! Do not forget to update messagingDataVersion in VersionInfo.ps1 when this parameter is updated !
    ///
    /// Increment BY TWO when:
    ///     1. Internal messaging structures change and messages can no longer be successfully transferred among components.
    ///     2. Some other updates were performed and we need to inform worker nodes that they need to upgrade.
    ///     3. Version number (below) is increased.
    let messagingDataVersion = MessagingDataVersion 118


    /// ! Do not forget to update versionNumber in VersionInfo.ps1 when this parameter is updated !
    ///
    /// This is an overall system version.
    [<Literal>]
    let VersionNumberValue = "6.0.6"


    /// A base name, which controls the database name and a working folder name.
    /// It is loosely the same as the version number.
    /// It must be updated when the old version is still running (for days) but the new version needs to be deployed.
    [<Literal>]
    let ClmBaseName = "clm606"


    [<Literal>]
    let MsgSvcBaseName = "msg606"


    [<Literal>]
    let WorkerNodeSvcBaseName = "wns606"


    /// Default port on which messaging communication is performed.
    let defaultServicePort = 5000 + messagingDataVersion.value


    [<Literal>]
    let CopyrightInfo = "MIT License - Copyright Konstantin K. Konstantinov and Alisa F. Konstantinova © 2015 - 2021."


    type VersionNumber =
        | VersionNumber of string

        member this.value = let (VersionNumber v) = this in v


    let versionNumberValue = VersionNumber VersionNumberValue
