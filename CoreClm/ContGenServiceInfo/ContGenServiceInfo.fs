namespace ContGenServiceInfo

open System
open System.ServiceModel
open System.Threading

open Softellect.Sys.Core
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.AppSettings
open Softellect.Wcf.Common
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


    let contGenServiceAddress = ConfigKey "ContGenServiceAddress"
    let contGenServiceHttpPort = ConfigKey "ContGenServiceHttpPort"
    let contGenServiceNetTcpPort = ConfigKey "ContGenServiceNetTcpPort"
    let contGenServiceCommunicationType = ConfigKey "ContGenServiceCommunicationType"

    // TODO kk:20201209 - Duplicate...
    let messagingServiceAddress = ConfigKey "MessagingServiceAddress"
    let messagingHttpServicePort = ConfigKey "MessagingHttpServicePort"
    let messagingNetTcpServicePort = ConfigKey "MessagingNetTcpServicePort"
    let messagingServiceCommunicationType = ConfigKey "MessagingServiceCommunicationType"

    let minUsefulEe = ConfigKey "MinUsefulEe"
    let partitionerId = ConfigKey "PartitionerId"
    let lastAllowedNodeErrInMinutes = ConfigKey "LastAllowedNodeErrInMinutes"
    let earlyExitCheckFrequencyInMinutes = ConfigKey "EarlyExitCheckFrequencyInMinutes"


    type ContGenSettings
        with

        member w.trySaveSettings() =
            match w.isValid(), AppSettingsProvider.tryCreate appSettingsFile with
            | Ok(), Ok provider ->
                let h = w.contGenSvcInfo.value.httpServiceInfo
                let n = w.contGenSvcInfo.value.netTcpServiceInfo

                let mh = w.messagingSvcInfo.messagingServiceAccessInfo.httpServiceInfo
                let mn = w.messagingSvcInfo.messagingServiceAccessInfo.netTcpServiceInfo

                try
                    provider.trySet contGenServiceAddress n.netTcpServiceAddress.value |> ignore
                    provider.trySet contGenServiceHttpPort h.httpServicePort.value |> ignore
                    provider.trySet contGenServiceNetTcpPort n.netTcpServicePort.value |> ignore
                    provider.trySet contGenServiceCommunicationType w.contGenCommType.value |> ignore

                    provider.trySet messagingServiceAddress mn.netTcpServiceAddress.value |> ignore
                    provider.trySet messagingHttpServicePort mh.httpServicePort.value |> ignore
                    provider.trySet messagingNetTcpServicePort mn.netTcpServicePort.value |> ignore
                    provider.trySet messagingServiceCommunicationType w.messagingCommType.value |> ignore

                    provider.trySet minUsefulEe w.contGenInfo.minUsefulEe.value |> ignore
                    provider.trySet partitionerId w.contGenInfo.partitionerId.value.value |> ignore
                    provider.trySet lastAllowedNodeErrInMinutes (w.contGenInfo.lastAllowedNodeErr.value / 1<minute>) |> ignore

                    Ok()
                with
                | e -> e |> ContGenSettingExn |> ContGenSettingsErr |> ContGenServiceErr |> Error
            | Error e, _ -> Error e
            | _, Error e -> e |> ContGenSettingExn |> ContGenSettingsErr |> ContGenServiceErr |> Error


    let loadContGenSettings() =
        let providerRes = AppSettingsProvider.tryCreate appSettingsFile

        let contGenInfo =
            match providerRes with
            | Ok provider ->
                {
                    minUsefulEe =
                        match provider.tryGetDecimal minUsefulEe with
                        | Ok (Some ee) -> ee |> double |> MinUsefulEe
                        | _ -> MinUsefulEe.defaultValue

                    partitionerId =
                        match provider.tryGetGuid partitionerId with
                        | Ok (Some p) when p <> Guid.Empty -> p |> MessagingClientId |> PartitionerId
                        | _ -> defaultPartitionerId

                    lastAllowedNodeErr =
                        match provider.tryGetInt lastAllowedNodeErrInMinutes with
                        | Ok (Some p) when p > 0 -> p * 1<minute> |> LastAllowedNodeErr
                        | _ -> LastAllowedNodeErr.defaultValue

                    earlyExitCheckFreq =
                        match provider.tryGetInt earlyExitCheckFrequencyInMinutes with
                        | Ok (Some p) when p > 0 -> p * 1<minute> |> EarlyExitCheckFreq
                        | _ -> EarlyExitCheckFreq.defaultValue
                }
            | _ ->
                {
                    minUsefulEe = MinUsefulEe.defaultValue
                    partitionerId = defaultPartitionerId
                    lastAllowedNodeErr = LastAllowedNodeErr.defaultValue
                    earlyExitCheckFreq = EarlyExitCheckFreq.defaultValue
                }

        let contGenServiceAddress =
            match providerRes with
            | Ok provider ->
                match provider.tryGetString contGenServiceAddress with
                | Ok (Some EmptyString) -> defaultContGenServiceAddress
                | Ok (Some s) -> s
                | _ -> defaultContGenServiceAddress
            | _ -> defaultContGenServiceAddress
            |> ServiceAddress

        let contGenServiceHttpPort =
            match providerRes with
            | Ok provider ->
                match provider.tryGetInt contGenServiceHttpPort with
                | Ok (Some n) when n > 0 -> n
                | _ -> defaultContGenHttpServicePort
            | _ -> defaultContGenHttpServicePort
            |> ServicePort

        let contGenServiceNetTcpPort =
            match providerRes with
            | Ok provider ->
                match provider.tryGetInt contGenServiceNetTcpPort with
                | Ok (Some n) when n > 0 -> n
                | _ -> defaultContGenNetTcpServicePort
            | _ -> defaultContGenNetTcpServicePort
            |> ServicePort

        let getCommunicationType s = WcfCommunicationType.tryCreate s |> Option.defaultValue NetTcpCommunication

        let contGenServiceCommunicationType =
            match providerRes with
            | Ok provider ->
                match provider.tryGetString contGenServiceCommunicationType with
                | Ok (Some s) -> getCommunicationType s
                | _ -> NetTcpCommunication
            | _ -> NetTcpCommunication

        let contGenSvcInfo = ContGenServiceAccessInfo.create contGenServiceAddress contGenServiceHttpPort contGenServiceNetTcpPort

        let messagingServiceCommunicationType =
            match providerRes with
            | Ok provider ->
                match provider.tryGetString messagingServiceCommunicationType with
                | Ok (Some s) -> getCommunicationType s
                | _ -> NetTcpCommunication
            | _ -> NetTcpCommunication

        let serviceAddress =
            match providerRes with
            | Ok provider ->
                match provider.tryGetString messagingServiceAddress with
                | Ok (Some EmptyString) -> defaultMessagingServiceAddress
                | Ok (Some s) -> s
                | _ -> defaultMessagingServiceAddress
            | _ -> defaultMessagingServiceAddress
            |> ServiceAddress

        let httpServicePort =
            match providerRes with
            | Ok provider ->
                match provider.tryGetInt messagingHttpServicePort with
                | Ok (Some n) when n > 0 -> n
                | _ -> defaultMessagingHttpServicePort
            | _ -> defaultMessagingHttpServicePort
            |> ServicePort

        let netTcpServicePort =
            match providerRes with
            | Ok provider ->
                match provider.tryGetInt messagingNetTcpServicePort with
                | Ok (Some n) when n > 0 -> n
                | _ -> defaultMessagingNetTcpServicePort
            | _ -> defaultMessagingNetTcpServicePort
            |> ServicePort

        let h = HttpServiceAccessInfo.create serviceAddress httpServicePort messagingHttpServiceName.value
        let n = NetTcpServiceAccessInfo.create serviceAddress netTcpServicePort messagingNetTcpServiceName.value
        let m = ServiceAccessInfo.create h n
        let messagingSvcInfo = MessagingServiceAccessInfo.create messagingDataVersion m

        let w =
            {
                contGenInfo = contGenInfo
                contGenSvcInfo = contGenSvcInfo
                contGenCommType = contGenServiceCommunicationType
                messagingSvcInfo = messagingSvcInfo
                messagingCommType = messagingServiceCommunicationType
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
