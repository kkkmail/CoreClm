namespace MessagingService

open MessagingServiceInfo.ServiceInfo
open Softellect.MessagingService.Program
open Primitives.VersionInfo

module Program =

    [<EntryPoint>]
    let main args = main<MessagingServiceData> "MessagingService" messagingDataVersion args
