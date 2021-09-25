namespace ClmSys

open Softellect.Sys.MessagingPrimitives

module VersionInfo =

    /// ! Do not forget to update messagingDataVersion in VersionInfo.ps1 when this parameter is updated !
    ///
    /// Increment BY TWO when:
    ///     1. Internal messaging structures change and messages can no longer be successfully transferred among components.
    ///     2. Some other updates were performed and we need to inform worker nodes that they need to upgrade.
    ///     3. Version number (below) is increased.
    let messagingDataVersion = MessagingDataVersion 122


    /// ! Do not forget to update versionNumber in VersionInfo.ps1 when this parameter is updated !
    ///
    /// This is an overall system version.
    [<Literal>]
    let VersionNumberValue = "6.0.8"


    /// The same as above but without the dots in order to use in database and folder names.
    [<Literal>]
    let private VersionNumberNumericalValue = "608"


    /// A base name, which controls the database name and a working folder name.
    [<Literal>]
    let ClmBaseName = "clm" + VersionNumberNumericalValue


    [<Literal>]
    let MsgSvcBaseName = "msg" + VersionNumberNumericalValue


    [<Literal>]
    let WorkerNodeSvcBaseName = "wns" + VersionNumberNumericalValue


    /// Default port on which messaging communication is performed.
    let defaultServicePort = 5000 + messagingDataVersion.value


    [<Literal>]
    let CopyrightInfo = "MIT License - Copyright Konstantin K. Konstantinov and Alisa F. Konstantinova © 2015 - 2021."


    type VersionNumber =
        | VersionNumber of string

        member this.value = let (VersionNumber v) = this in v


    let versionNumberValue = VersionNumber VersionNumberValue
