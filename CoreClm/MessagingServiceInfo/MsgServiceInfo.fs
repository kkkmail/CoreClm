namespace MessagingServiceInfo

open System
open System.ServiceModel
open FSharp.Configuration

open Softellect.Sys.Core
open Softellect.Sys.Primitives
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.Logging
open Softellect.Sys.MessagingErrors
open Softellect.Wcf.Common
open Softellect.Wcf.Service
open Softellect.Messaging.Primitives
open Softellect.Messaging.ServiceInfo
open Softellect.Messaging.Service
open Softellect.Messaging.Client
open Softellect.Messaging.Proxy
open Softellect.Sys.MessagingClientErrors
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
open ClmSys.MessagingPrimitives
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

//        member this.keepInMemory() =
//            match this.getMessageSize() with
//            | SmallSize -> true
//            | MediumSize -> false
//            | LargeSize -> false

//        member this.getInfo() =
//            let s = (sprintf "%A" this)
//            s.Substring(0, min s.Length MessageData.maxInfoLength)


//    type MessageRecipientInfo =
//        {
//            recipient : MessagingClientId
//            deliveryType : MessageDeliveryType
//        }
//
//
//    type MessageInfo =
//        {
//            recipientInfo : MessageRecipientInfo
//            messageData : MessageData
//        }


    type MessagingClient = MessagingClient<ClmMessageData, ClmError>
    type MessagingClientData = MessagingClientData<ClmMessageData, ClmError>
    type MessagingServiceData = MessagingServiceData<ClmMessageData, ClmError>
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


    //type MessageType =
    //    | IncomingMessage
    //    | OutgoingMessage


    ///// TODO kk:20190930 - The name is not good.
    //type MessageDataInfo =
    //    {
    //        messageId : MessageId
    //        dataVersion : MessagingDataVersion
    //        sender : MessagingClientId
    //        recipientInfo : MessageRecipientInfo
    //        createdOn : DateTime
    //    }


    //type Message =
    //    {
    //        messageDataInfo : MessageDataInfo
    //        messageData : MessageData
    //    }


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


    type MsgWcfSvcShutDownInfo =
        {
            //serviceHost : ServiceHost
            serviceHost : int
        }


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


    type IMessagingService =
        abstract getVersion : unit -> ClmResult<MessagingDataVersion>
        abstract sendMessage : Message -> UnitResult
        abstract tryPeekMessage : MessagingClientId -> ClmResult<Message option>
        abstract tryDeleteFromServer : (MessagingClientId * MessageId) -> UnitResult


    /// https://gist.github.com/dgfitch/661656
    [<ServiceContract(ConfigurationName = MessagingWcfServiceName)>]
    type IMessagingWcfService =

        [<OperationContract(Name = "getVersion")>]
        abstract getVersion : u:byte[] -> byte[]

        [<OperationContract(Name = "sendMessage")>]
        abstract sendMessage : m:byte[] -> byte[]

        [<OperationContract(Name = "tryPeekMessage")>]
        abstract tryPeekMessage : c:byte[] -> byte[]

        [<OperationContract(Name = "tryDeleteFromServer")>]
        abstract tryDeleteFromServer : cm:byte[] -> byte[]


    type WcfCommunicator = (IMessagingWcfService-> byte[] -> byte[])


    [<Literal>]
    let MsgAppConfigFile = __SOURCE_DIRECTORY__ + @"\..\MessagingService\app.config"


    type MsgAppSettings = AppSettings<MsgAppConfigFile>


    type MsgSettings
        with
        member w.trySaveSettings() =
            match w.isValid() with
            | Ok() ->
                try
                    MsgAppSettings.MsgSvcAddress <- w.messagingSvcInfo.messagingServiceAddress.value.value
                    MsgAppSettings.MsgSvcPort <- w.messagingSvcInfo.messagingServicePort.value.value
                    MsgAppSettings.ExpirationTimeInMinutes <- int w.messagingInfo.expirationTime.TotalMinutes

                    Ok()
                with
                | e -> e |> MsgSettingExn |> MsgSettingsErr |> MessagingServiceErr |> Error
            | Error e -> Error e


    let loadMsgServiceSettings() =
        MsgAppSettings.SelectExecutableFile(getFileName messagingProgramName)

        {
            messagingInfo =
                {
                    expirationTime = TimeSpan.FromMinutes(float MsgAppSettings.ExpirationTimeInMinutes)
                }

            messagingSvcInfo =
                {
                    messagingServiceAddress =
                        match MsgAppSettings.MsgSvcAddress with
                        | EmptyString -> MessagingServiceAddress.defaultValue
                        | s -> s |> ServiceAddress |> MessagingServiceAddress

                    messagingServicePort =
                        match MsgAppSettings.MsgSvcPort with
                        | n  when n > 0 -> n |> ServicePort |> MessagingServicePort
                        | _ -> MessagingServicePort.defaultValue

                    messagingServiceName = messagingServiceName
                }
        }


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
