namespace ClmSys

open VersionInfo

module MessagingCommonErrors =

    type VersionMismatchInfo =
        {
            localVersion : MessagingDataVersion
            remoteVersion : MessagingDataVersion
        }
