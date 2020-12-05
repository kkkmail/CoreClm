namespace MessagingService

open System.Configuration.Install
open System.ComponentModel
open System.ServiceProcess
open MessagingService.MsgWindowsService
open ClmSys.MessagingPrimitives

[<RunInstaller(true)>]
type MsgServiceInstaller() =
    inherit Installer()

    do
        // Specify properties of the hosting process.
        new ServiceProcessInstaller(Account = ServiceAccount.LocalSystem)
        |> base.Installers.Add |> ignore

        // Specify properties of the service running inside the process.
        new ServiceInstaller
          ( DisplayName = messagingServiceName.value.value,
            ServiceName = messagingServiceName.value.value,
            StartType = ServiceStartMode.Automatic )
        |> base.Installers.Add |> ignore


// Run the services when the process starts.
module Main = ServiceBase.Run [| new MessagingWindowsService() :> ServiceBase |]
