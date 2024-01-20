namespace MessagingServiceInfo

open System

open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives
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
open Primitives.VersionInfo
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
open Primitives.GeneralData
open ClmSys.SolverData

module ServiceInfo =

    let messagingProgramName = "MessagingService.exe"


    [<Literal>]
    let MessagingWcfServiceName = "MessagingWcfService"


    type PartitionerMessage =
        | UpdateProgressPrtMsg of ProgressUpdateInfo
        | SaveChartsPrtMsg of ChartInfo
        | RegisterWorkerNodePrtMsg of WorkerNodeInfo
        | UnregisterWorkerNodePrtMsg of WorkerNodeId

        member this.messageSize =
            match this with
            | UpdateProgressPrtMsg _ -> SmallSize
            | SaveChartsPrtMsg _ -> MediumSize
            | RegisterWorkerNodePrtMsg _ -> SmallSize
            | UnregisterWorkerNodePrtMsg _ -> SmallSize


    type EarlyExitData = ChartData


    let bindBool b s =
        match b with
        | true -> b, Some s
        | false -> b, None


    type EarlyExitRule =
        | ProgressExceeds of decimal
        | MaxWeightedAverageAbsEeExceeds of float
        | MaxLastEeExceeds of float
        | MaxAverageEeExceeds of float
        | RunTimeExceeds of TimeSpan

        member r.isValid (d : EarlyExitData) =
            match r with
            | ProgressExceeds p -> d.progress > p, $"progress: %A{d.progress} > %A{p}"
            | MaxWeightedAverageAbsEeExceeds e ->
                d.eeData.maxWeightedAverageAbsEe > e, $"maxWeightedAverageAbsEe: %A{d.eeData.maxWeightedAverageAbsEe} > %A{e}"
            | MaxLastEeExceeds e -> d.eeData.maxLastEe > e, $"maxLastEe: %A{d.eeData.maxLastEe} > %A{e}"
            | MaxAverageEeExceeds e -> d.eeData.maxAverageEe > e, $"maxAverageEe: %A{d.eeData.maxAverageEe} > %A{e}"
            | RunTimeExceeds e -> (DateTime.Now - d.startedOn) > e, $"runTime exceeds: {e}"
            ||> bindBool


    /// Any rule can be satisfied.
    type AnyRule =
        | AnyRule of list<EarlyExitRule>

        member e.value = let (AnyRule v) = e in v


    /// Outer list - all collections must be satisfied, inner list (from AnyRule) - at least one rule must be satisfied.
    type EarlyExitRuleCollection =
        | AllOfAny of list<AnyRule>


    type EarlyExitStrategy =
        | AnyRuleCollection of list<EarlyExitRuleCollection> // Any of the collections can be satisfied.

        member e.exitEarly d =
            match e with
            | AnyRuleCollection c ->
                match c with
                | [] -> false, None // If there are no collections, then early exit strategy cannot work.
                | _ ->
                    let r v =
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

                            let foldInner (a : AnyRule) =
                                a.value |> List.fold (fun acc b -> combineOr (b.isValid d) acc) (false, None)

                            let combineAnd (r1, m1) (r2, m2) = r1 && r2, g m1 m2

                            let result =
                                v
                                |> List.map foldInner
                                |> List.fold combineAnd (true, None)

                            result

                    let chooser (AllOfAny v) =
                        let x = r v
                        match x with
                        | false, _ -> None
                        | true, _ -> Some x

                    c |> List.tryPick chooser |> Option.defaultValue (false, None)

        static member getEeValue p e =
            [
                [
                    ProgressExceeds p
                ]
                |> AnyRule

                [
                    MaxWeightedAverageAbsEeExceeds e
                    MaxLastEeExceeds e
                    MaxAverageEeExceeds e
                ]
                |> AnyRule
            ]
            |> AllOfAny

        static member quickValue p =
                EarlyExitStrategy.getEeValue p.quickProgress p.quickMinEe

        static member standardValue p =
                EarlyExitStrategy.getEeValue p.standardProgress p.standardMinEe

        static member slowValue p =
                EarlyExitStrategy.getEeValue p.slowProgress p.slowMinEe

        static member longRunningValue p =
            [
                [ RunTimeExceeds p.maxRunTime ] |> AnyRule
            ]
            |> AllOfAny

        static member getValue p =
            [
                EarlyExitStrategy.quickValue
                EarlyExitStrategy.standardValue
                EarlyExitStrategy.slowValue
                EarlyExitStrategy.longRunningValue
            ]
            |> List.map (fun e -> e p)
            |> AnyRuleCollection


    type EarlyExitInfo =
        {
            frequency : EarlyExitCheckFrequency
            earlyExitStrategy : EarlyExitStrategy
        }

        static member getValue p =
            {
                frequency = p.earlyExitCheckFreq
                earlyExitStrategy = EarlyExitStrategy.getValue p
            }


    type WorkerNodeRunModelData =
        {
            runningProcessData : RunningProcessData
            modelData : ModelData
            controlData : RunnerControlData
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


        member q.toMessageInfoOpt getModelData cd =
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
                                modelData = m
                                controlData = cd
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
        let messagingSvcInfo, messagingServiceCommunicationType = loadMessagingSettings providerRes

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
        printfn $"getServiceAccessInfoImpl: w = %A{w}"

        let r =
            match tryGetSaveSettings(), b with
            | Some _, _ -> w.trySaveSettings()
            | _, true -> w.trySaveSettings()
            | _ -> Ok()

        match r with
        | Ok() -> printfn "Successfully saved settings."
        | Error e -> printfn $"Error occurred trying to save settings: %A{e}."

        w
