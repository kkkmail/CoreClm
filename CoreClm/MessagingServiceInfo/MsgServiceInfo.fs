namespace MessagingServiceInfo

open System

open Softellect.Sys
open Softellect.Sys.Primitives
open Softellect.Sys.AppSettings
open Softellect.Wcf.Common
open Softellect.Wcf.Service
open Softellect.Messaging.Primitives
open Softellect.Messaging.ServiceInfo
open Softellect.Messaging.Service
open Softellect.Messaging.Client
open Softellect.Sys.MessagingServiceErrors

open ClmSys.MessagingData
open ClmSys.SolverRunnerPrimitives
open ClmSys.VersionInfo
open ClmSys.WorkerNodeData
open ContGenServiceInfo.ServiceInfo
open Clm.CalculationData
open Clm.ModelParams
open ClmSys.ContGenPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.ClmErrors
open ClmSys.GeneralPrimitives
open Clm.ChartData
open ClmSys.GeneralData

module ServiceInfo =

    let messagingProgramName = "MessagingService.exe"


    [<Literal>]
    let MessagingWcfServiceName = "MessagingWcfService"


    type PartitionerMessage =
        | UpdateProgressPrtMsg of ProgressUpdateInfo
        | SaveResultPrtMsg of ResultDataWithId
        | SaveChartsPrtMsg of ChartInfo
        | RegisterWorkerNodePrtMsg of WorkerNodeInfo
        | UnregisterWorkerNodePrtMsg of WorkerNodeId

        member this.messageSize =
            match this with
            | UpdateProgressPrtMsg _ -> SmallSize
            | SaveResultPrtMsg _ -> SmallSize
            | SaveChartsPrtMsg _ -> MediumSize
            | RegisterWorkerNodePrtMsg _ -> SmallSize
            | UnregisterWorkerNodePrtMsg _ -> SmallSize


    type EarlyExitData = ChartData


    let bindBool s b =
        match b with
        | true -> b, Some s
        | false -> b, None


    type EarlyExitRule =
        | ProgressExceeds of decimal
        | MaxWeightedAverageAbsEeExceeds of float
        | MaxLastEeExceeds of float
        | MaxAverageEeExceeds of float

        member r.isValid (d : EarlyExitData) =
            match r with
            | ProgressExceeds p -> d.progress > p |> bindBool (sprintf "progress: %A > %A" d.progress p)
            | MaxWeightedAverageAbsEeExceeds e ->
                d.maxWeightedAverageAbsEe > e |> bindBool (sprintf "maxWeightedAverageAbsEe: %A > %A" d.maxWeightedAverageAbsEe e)
            | MaxLastEeExceeds e -> d.maxLastEe > e |> bindBool (sprintf "maxLastEe: %A > %A" d.maxLastEe e)
            | MaxAverageEeExceeds e -> d.maxAverageEe > e |> bindBool (sprintf "maxAverageEe: %A > %A" d.maxAverageEe e)


    type EarlyExitCheckFrequency =
        | EarlyExitCheckFrequency of TimeSpan

        member this.value = let (EarlyExitCheckFrequency v) = this in v

        static member defaultValue = TimeSpan.FromHours(1.0) |> EarlyExitCheckFrequency


    type EarlyExitStrategy =
        | AllOfAny of list<list<EarlyExitRule>> // Outer list - all collections must be satisfied, inner list - at least one rule must be satisfied.

        member e.exitEarly d =
            match e with
            |AllOfAny v ->
                match v with
                | [] -> false, None // If outer list is empty, then early exit strategy cannot work.
                | _ ->
                    let g m1 m2 =
                        match m1, m2 with
                        | Some m1, Some m2 -> m1 + ", " + m2 |> Some
                        | Some m1, None -> Some m1
                        | None, Some m2 -> Some m2
                        | None, None -> None

                    let combineOr (r1, m1) (r2, m2) = r1 || r2, g m1 m2

                    let foldInner (a : list<EarlyExitRule>) =
                        a |> List.fold (fun acc b -> combineOr (b.isValid d) acc) (false, None)

                    let combineAnd (r1, m1) (r2, m2) = r1 && r2, g m1 m2

                    let r =
                        v
                        |> List.map foldInner
                        |> List.fold combineAnd (true, None)

                    r

        static member defaultProgress = 0.05M
        static member defaultMinEe = 0.15

        static member getDefaultValue p e =
            [
                [
                    ProgressExceeds p
                ]
                [
                    MaxWeightedAverageAbsEeExceeds e
                    MaxLastEeExceeds e
                    MaxAverageEeExceeds e
                ]
            ]
            |> AllOfAny

            static member defaultValue = EarlyExitStrategy.getDefaultValue EarlyExitStrategy.defaultProgress EarlyExitStrategy.defaultMinEe


    type EarlyExitInfo =
        {
            frequency : EarlyExitCheckFrequency
            earlyExitStrategy : EarlyExitStrategy
        }

        static member getDefaultValue f p e =
            {
                frequency = f
                earlyExitStrategy = EarlyExitStrategy.getDefaultValue p e
            }

        static member defaultValue =
            {
                frequency = EarlyExitCheckFrequency.defaultValue
                earlyExitStrategy = EarlyExitStrategy.defaultValue
            }


    type WorkerNodeRunModelData =
        {
            runningProcessData : RunningProcessData
            modelData : ModelData
            minUsefulEe : MinUsefulEe
            earlyExitOpt : EarlyExitInfo option
        }


    type WorkerNodeMessage =
        | RunModelWrkMsg of WorkerNodeRunModelData
        | CancelRunWrkMsg of (RunQueueId * CancellationType)
        | RequestResultWrkMsg of (RunQueueId * ResultNotificationType)

        member this.messageSize =
            match this with
            | RunModelWrkMsg _ -> LargeSize
            | CancelRunWrkMsg _ -> SmallSize
            | RequestResultWrkMsg _ -> SmallSize


    /// The decision was that we want strongly typed messages rather than untyped messages.
    /// TextData is used mostly for tests but can be also used to send an arbitrary object serialized into JSON.
    type ClmMessageData =
        | TextData of string
        | PartitionerMsg of PartitionerMessage
        | WorkerNodeMsg of WorkerNodeMessage

        static member maxInfoLength = 500

        member this.getMessageSize() =
            match this with
            | TextData s ->
                if s.Length < 1_000 then SmallSize
                else if s.Length < 1_000_000 then MediumSize
                else LargeSize
            | PartitionerMsg m -> m.messageSize
            | WorkerNodeMsg m -> m.messageSize


    type MessagingClient = MessagingClient<ClmMessageData, ClmError>
    type MessagingClientData = MessagingClientData<ClmMessageData, ClmError>
    type MessagingServiceData = MessagingServiceData<ClmMessageData, ClmError>
    type MessagingWcfServiceData = WcfServiceData<MessagingServiceData<ClmMessageData, ClmError>>
    type Message = Message<ClmMessageData>
    type MessageInfo = MessageInfo<ClmMessageData>
    type MessagingService = MessagingService<ClmMessageData, ClmError>
    type MessagingWcfService = MessagingWcfService<ClmMessageData, ClmError>
    type MessagingWcfServiceImpl = WcfService<MessagingWcfService, IMessagingWcfService, MessagingServiceData>


    type PartitionerMessageInfo =
        {
            partitionerRecipient : PartitionerId
            deliveryType : MessageDeliveryType
            messageData : PartitionerMessage
        }

        member this.getMessageInfo() =
            {
                recipientInfo =
                    {
                        recipient = this.partitionerRecipient.messagingClientId
                        deliveryType = this.deliveryType
                    }
                messageData = this.messageData |> PartitionerMsg |> UserMsg
            }


    type WorkerNodeMessageInfo =
        {
            workerNodeRecipient : WorkerNodeId
            deliveryType : MessageDeliveryType
            messageData : WorkerNodeMessage
        }

        member this.getMessageInfo() =
            {
                recipientInfo =
                    {
                        recipient = this.workerNodeRecipient.messagingClientId
                        deliveryType = this.deliveryType
                    }
                messageData = this.messageData |> WorkerNodeMsg |> UserMsg
            }


    type MessageWithOptionalData =
        {
            messageDataInfo : MessageDataInfo
            messageDataOpt : ClmMessageData option
        }


    type MessageDataInfo
        with
        member this.isExpired(waitTime : TimeSpan) =
            match this.recipientInfo.deliveryType with
            | GuaranteedDelivery -> false
            | NonGuaranteedDelivery -> if this.createdOn.Add waitTime < DateTime.Now then true else false


    type MessagingConfigParam =
        | DummyConfig


    type RunQueue
        with

        member q.toRunningProcessDataOpt() =
            q.workerNodeIdOpt
            |> Option.bind (fun w ->
                            {
                                modelDataId = q.info.modelDataId
                                defaultValueId = q.info.defaultValueId
                                runQueueId = q.runQueueId
                                workerNodeId = w
                                commandLineParams = q.modelCommandLineParam
                            }
                            |> Some)


        member q.toMessageInfoOpt getModelData minUsefulEe eeo =
            match q.toRunningProcessDataOpt() with
            | Some d ->
                match getModelData q.info.modelDataId with
                | Ok m ->
                    {
                        workerNodeRecipient = d.workerNodeId
                        deliveryType = GuaranteedDelivery
                        messageData =
                            {
                                runningProcessData = d
                                minUsefulEe = minUsefulEe
                                modelData = m
                                earlyExitOpt = eeo
                            }
                            |> RunModelWrkMsg
                    }.getMessageInfo()
                    |> Some |> Ok
                | Error e -> Error e
            | None -> Ok None


    let expirationTimeInMinutes = ConfigKey "ExpirationTimeInMinutes"


    type MsgSettings
        with

        member w.trySaveSettings() =
            let toErr e = e |> MsgSettingExn |> MsgSettingsErr |> MessagingServiceErr |> Error

            match w.isValid(), AppSettingsProvider.tryCreate appSettingsFile with
            | Ok(), Ok provider ->
                try
                    updateMessagingSettings provider w.messagingSvcInfo w.communicationType
                    provider.trySet expirationTimeInMinutes (int w.messagingInfo.expirationTime.TotalMinutes) |> ignore
                    provider.trySave() |> Rop.bindError toErr
                with
                | e -> toErr e
            | Error e, _ -> Error e
            | _, Error e -> toErr e


    type MsgServiceSettingsProxy<'P> =
        {
            tryGetMsgServiceAddress : 'P -> ServiceAddress option
            tryGetMsgServicePort : 'P -> ServicePort option
        }

    let loadMsgServiceSettings() =
        let providerRes = AppSettingsProvider.tryCreate appSettingsFile
        let (messagingSvcInfo, messagingServiceCommunicationType) = loadMessagingSettings providerRes

        let expirationTimeInMinutes =
            match providerRes with
            | Ok provider ->
                match provider.tryGetInt expirationTimeInMinutes with
                | Ok (Some n) when n > 0 -> TimeSpan.FromMinutes(float n)
                | _ -> MessagingServiceInfo.defaultExpirationTime
            | _ -> MessagingServiceInfo.defaultExpirationTime

        let w =
            {
                messagingInfo =
                    {
                        expirationTime = expirationTimeInMinutes
                        messagingDataVersion = messagingDataVersion
                    }

                messagingSvcInfo = messagingSvcInfo
                communicationType = messagingServiceCommunicationType
            }

        w


    let loadSettingsImpl (proxy : MsgServiceSettingsProxy<'P>) p =
        let w = loadMsgServiceSettings()
        let h = w.messagingSvcInfo.messagingServiceAccessInfo.httpServiceInfo
        let n = w.messagingSvcInfo.messagingServiceAccessInfo.netTcpServiceInfo

        let serviceAddress = proxy.tryGetMsgServiceAddress p |> Option.defaultValue h.httpServiceAddress
        let netTcpServicePort = proxy.tryGetMsgServicePort p |> Option.defaultValue n.netTcpServicePort
        let httpServiceInfo = HttpServiceAccessInfo.create serviceAddress h.httpServicePort h.httpServiceName
        let netTcpServiceInfo = NetTcpServiceAccessInfo.create serviceAddress netTcpServicePort n.netTcpServiceName WcfSecurityMode.defaultValue
        let msgServiceAccessInfo = ServiceAccessInfo.create httpServiceInfo netTcpServiceInfo
        let messagingSvcInfo = MessagingServiceAccessInfo.create messagingDataVersion msgServiceAccessInfo

        let w1 = { w with messagingSvcInfo = messagingSvcInfo }

        w1


    let getMsgServiceInfo (loadSettings, tryGetSaveSettings) b =
        let (w : MsgSettings) = loadSettings()
        printfn "getServiceAccessInfoImpl: w = %A" w

        let r =
            match tryGetSaveSettings(), b with
            | Some _, _ -> w.trySaveSettings()
            | _, true -> w.trySaveSettings()
            | _ -> Ok()

        match r with
        | Ok() -> printfn "Successfully saved settings."
        | Error e -> printfn "Error occurred trying to save settings: %A." e

        w
