namespace ContGenServiceInfo

open System
open System.ServiceModel
open System.Threading
open FSharp.Configuration

open Softellect.Sys.Core
open Softellect.Sys.MessagingPrimitives
open Softellect.Messaging.Primitives
open Softellect.Sys.Core
open Softellect.Sys.ServiceInstaller
open Softellect.Sys.Primitives
open Softellect.Sys.Core
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.MessagingServiceErrors
open Softellect.Messaging.ServiceInfo

open ClmSys.VersionInfo
open ClmSys.MessagingPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.GeneralData
open Clm.ModelParams
open ClmSys.GeneralPrimitives
open ClmSys.SolverRunnerPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.ContGenPrimitives
open ClmSys.ClmErrors
open ClmSys.ContGenErrors
open ClmSys.ContGenData
open ClmSys.PartitionerData

module ServiceInfo =

    let contGenServiceProgramName = "ContGenService.exe"


    [<Literal>]
    let ContGenWcfServiceName = "ContGenWcfService"


    type RunningProcessData =
        {
            modelDataId : ModelDataId
            defaultValueId : ClmDefaultValueId
            runQueueId : RunQueueId
            workerNodeId : WorkerNodeId
            commandLineParams : ModelCommandLineParam
        }


    type ProgressUpdateInfo =
        {
            runQueueId : RunQueueId
            progress : TaskProgress
        }


    type RunningProcessInfo =
        {
            started : DateTime
            progressUpdateInfo : ProgressUpdateInfo
        }


    type ProgressUpdateInfo
        with
        member this.toRunningProcessInfo() =
            {
                started = DateTime.Now
                progressUpdateInfo = this
            }


    let mutable private callCount = -1


    let getServiceState (getState : unit -> (list<RunQueue> * UnitResult)) =
        if Interlocked.Increment(&callCount) = 0
        then
            try
                printfn "Getting state at %s ..." (DateTime.Now.ToString("yyyy-MM-dd.HH:mm:ss"))
                let (q, e) = getState()
                let r0 = q |> List.sortBy (fun e -> e.progress) |> List.map (fun e -> "      " + e.ToString()) |> String.concat Nl
                let r = if r0 = EmptyString then "[]" else Nl + "    [" + Nl + r0 + Nl + "    ]"
                printfn "... state at %s\n{\n  running = %s\n  runningCount = %A\n }"  (DateTime.Now.ToString("yyyy-MM-dd.HH:mm:ss")) r q.Length
            with
            | e -> printfn "Exception occurred: %A" e
        else
            printfn "Not getting state at %A because callCount = %A." DateTime.Now callCount
            ignore()

        Interlocked.Decrement(&callCount) |> ignore
        Ok()


    type IContGenService =
        abstract tryCancelRunQueue : RunQueueId -> CancellationType -> UnitResult
        abstract tryRequestResults : RunQueueId -> ResultNotificationType -> UnitResult


    /// https://gist.github.com/dgfitch/661656
    [<ServiceContract(ConfigurationName = ContGenWcfServiceName)>]
    type IContGenWcfService =

        [<OperationContract(Name = "tryCancelRunQueue")>]
        abstract tryCancelRunQueue : q:byte[] -> byte[]


        [<OperationContract(Name = "tryRequestResults")>]
        abstract tryRequestResults : q:byte[] -> byte[]


    [<Literal>]
    let ContGenAppConfigFile = __SOURCE_DIRECTORY__ + @"\..\ContGenService\app.config"


    type ContGenAppSettings = AppSettings<ContGenAppConfigFile>


    type ContGenSettings
        with
        member w.trySaveSettings() =
            match w.isValid() with
            | Ok() ->
                try
                    ContGenAppSettings.ContGenSvcAddress <- w.contGenSvcInfo.contGenServiceAddress.value.value
                    ContGenAppSettings.ContGenSvcPort <- w.contGenSvcInfo.contGenServicePort.value.value

                    ContGenAppSettings.MsgSvcAddress <- w.messagingSvcInfo.messagingServiceAccessInfo.serviceAddress.value
                    ContGenAppSettings.MsgSvcPort <- w.messagingSvcInfo.messagingServiceAccessInfo.netTcpServicePort.value

                    ContGenAppSettings.MinUsefulEe <- w.contGenInfo.minUsefulEe.value
                    ContGenAppSettings.PartitionerId <- w.contGenInfo.partitionerId.value.value
                    ContGenAppSettings.LastAllowedNodeErrInMinutes <- w.contGenInfo.lastAllowedNodeErr.value / 1<minute>

                    Ok()
                with
                | e -> e |> ContGenSettingExn |> ContGenSettingsErr |> ContGenServiceErr |> Error
            | Error e -> Error e


    let loadContGenSettings() =
        ContGenAppSettings.SelectExecutableFile(getFileName contGenServiceProgramName)

        let w =
            {
                contGenInfo =
                    {
                        minUsefulEe = ContGenAppSettings.MinUsefulEe |> MinUsefulEe

                        partitionerId =
                            match ContGenAppSettings.PartitionerId with
                            | p when p <> Guid.Empty -> p |> MessagingClientId |> PartitionerId
                            | _ -> defaultPartitionerId

                        lastAllowedNodeErr =
                            match ContGenAppSettings.LastAllowedNodeErrInMinutes with
                            | p when p > 0 -> p * 1<minute> |> LastAllowedNodeErr
                            | _ -> LastAllowedNodeErr.defaultValue

                        earlyExitCheckFreq =
                            match ContGenAppSettings.EarlyExitCheckFrequencyInMinutes with
                            | p when p > 0 -> p * 1<minute> |> EarlyExitCheckFreq
                            | _ -> EarlyExitCheckFreq.defaultValue
                    }

                contGenSvcInfo =
                    {
                        contGenServiceAddress =
                            match ContGenAppSettings.ContGenSvcAddress with
                            | EmptyString -> ContGenServiceAddress.defaultValue
                            | s -> s |> ServiceAddress |> ContGenServiceAddress

                        contGenServicePort =
                            match ContGenAppSettings.ContGenSvcPort with
                            | n when n > 0 -> n |> ServicePort |> ContGenServicePort
                            | _ -> ContGenServicePort.defaultValue

                        contGenServiceName = contGenServiceName
                    }

                messagingSvcInfo =
                    {
                        messagingServiceAccessInfo =
                            {
                                serviceAddress =
                                    match ContGenAppSettings.MsgSvcAddress with
                                    | EmptyString -> defaultMessagingServiceAddress
                                    | s -> s
                                    |> ServiceAddress

                                httpServicePort = 0
                                httpServiceName = 0

                                netTcpServicePort =
                                    match ContGenAppSettings.MsgSvcPort with
                                    | n  when n > 0 -> n
                                    | _ -> defaultMessagingNetTcpServicePort
                                    |> ServicePort

                                netTcpServiceName = messagingServiceName
                            }
                        messagingDataVersion = messagingDataVersion
                    }
            }
        w


    let saveContGenSettings loadSettings tryGetSaveSettings =
        let (w : ContGenSettings) = loadSettings()

        let r =
            match tryGetSaveSettings() with
            | Some() -> w.trySaveSettings()
            | None -> Ok()

        match r with
        | Ok() -> printfn "Successfully saved settings."
        | Error e -> printfn "Error occurred trying to save settings: %A." e
