﻿namespace WorkerNodeService

open System
open Argu

open Softellect.Sys.Core
open Softellect.Sys
open Softellect.Messaging.ServiceInfo
open Softellect.Wcf.Common
open Softellect.Messaging.Primitives
open Softellect.Messaging.Client
open Softellect.Messaging.Proxy
open Softellect.Sys.Rop

open ClmSys.Logging
open ClmSys.WorkerNodeData
open ClmSys.TimerEvents
open ContGenServiceInfo.ServiceInfo
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.SvcCommandLine
open ServiceProxy.MsgServiceProxy
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.WorkerNodeProxy
open ServiceProxy.MsgProcessorProxy
open ClmSys.ClmErrors
open ClmSys.GeneralPrimitives
open ClmSys.WorkerNodeErrors
open ClmSys.WorkerNodePrimitives
open ServiceProxy.SolverProcessProxy
open DbData.Configuration

module ServiceImplementation =

    let private toError g f = f |> g |> WorkerNodeErr |> Error
    let private addError g f e = ((f |> g |> WorkerNodeErr) + e) |> Error


    let mutable serviceAccessInfo =
        let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = workerNodeServiceProgramName)
        let results = (parser.Parse [||]).GetAllResults()
        results |> getServiceAccessInfo


    type WorkerNodeMessageResult = MessageProcessorResult<WorkerNodeRunnerState * UnitResult, ClmError>


    type WorkerNodeRunnerState
        with
        static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]

        static member defaultValue =
            {
                workerNodeState = NotStartedWorkerNode
            }


    type WorkerNodeRunnerData =
        {
            workerNodeServiceInfo : WorkerNodeServiceInfo
            workerNodeProxy : WorkerNodeProxy
            messageProcessorProxy : MessageProcessorProxy
        }


    let onRegister (proxy : OnRegisterProxy) s =
        let result =
            {
                partitionerRecipient = proxy.sendMessageProxy.partitionerId
                deliveryType = GuaranteedDelivery
                messageData = proxy.workerNodeInfo |> RegisterWorkerNodePrtMsg
            }.getMessageInfo()
            |> proxy.sendMessageProxy.sendMessage

        s, result


    let onUnregister (proxy : OnRegisterProxy) s =
        let result =
            {
                partitionerRecipient = proxy.sendMessageProxy.partitionerId
                deliveryType = GuaranteedDelivery
                messageData = proxy.workerNodeInfo.workerNodeId |> UnregisterWorkerNodePrtMsg
            }.getMessageInfo()
            |> proxy.sendMessageProxy.sendMessage

        s, result


    /// TODO kk:20210511 - At this point having a "state" for a worker node seems totally useless
    /// because now we attempt to restart everything on a [lengthy] timer event. This is to account for NOT
    /// started solvers due to node overload.
    let onStart (proxy : OnStartProxy) s =
        let g() =
            match proxy.loadAllActiveRunQueueId() with
            | Ok m -> m |> List.map proxy.onRunModel |> foldUnitResults
            | Error e -> Error e

        match s.workerNodeState with
        | NotStartedWorkerNode ->
            let w = { s with workerNodeState = StartedWorkerNode }
            w, g()
        | StartedWorkerNode -> s, g()


    let onProcessMessage (proxy : OnProcessMessageProxy) (m : Message) =
        printfn $"onProcessMessage: Starting. messageId: {m.messageDataInfo.messageId}."

        match m.messageData with
        | UserMsg (WorkerNodeMsg x) ->
            match x with
            | RunModelWrkMsg d ->
                printfn $"    onProcessMessage: runQueueId: {d.runningProcessData.runQueueId}."

                match proxy.saveWorkerNodeRunModelData d with
                | Ok() ->
                    printfn $"    onProcessMessage: saveWorkerNodeRunModelData with runQueueId: {d.runningProcessData.runQueueId} - OK."
                    let result = proxy.onRunModel d.runningProcessData.runQueueId
                    printfn $"    onProcessMessage: onRunModel with runQueueId: {d.runningProcessData.runQueueId} - %A{result}."
                    result
                | Error e ->
                    printfn $"    onProcessMessage: saveWorkerNodeRunModelData with runQueueId: {d.runningProcessData.runQueueId} ERROR: %A{e}."
                    addError OnProcessMessageErr (CannotSaveModelDataErr (m.messageDataInfo.messageId, d.runningProcessData.runQueueId)) e
            | CancelRunWrkMsg q -> q ||> proxy.requestCancellation
            | RequestResultWrkMsg q -> q ||> proxy.notifyOfResults
        | _ -> (m.messageDataInfo.messageId, m.messageData.getInfo()) |> InvalidMessageErr |> toError OnProcessMessageErr


    type OnGetMessagesProxy = OnGetMessagesProxy<WorkerNodeRunnerState>
    let private onGetMessages = onGetMessages<WorkerNodeRunnerState>
    type OnConfigureWorkerProxy = OnRegisterProxy

    let onGetState (s : WorkerNodeRunnerState) =
        failwith "onGetState is not implemented yet."


    let sendMessageProxy i =
        {
            partitionerId = i.workerNodeServiceInfo.workerNodeInfo.partitionerId
            sendMessage = i.messageProcessorProxy.sendMessage
        }


    let onRegisterProxy i : OnRegisterProxy =
        {
            workerNodeInfo = i.workerNodeServiceInfo.workerNodeInfo
            sendMessageProxy = sendMessageProxy i
        }


    type WorkerNodeMessage =
        | Start of OnStartProxy * AsyncReplyChannel<UnitResult>
        | Register of AsyncReplyChannel<UnitResult>
        | Unregister of AsyncReplyChannel<UnitResult>
        | GetMessages of OnGetMessagesProxy * AsyncReplyChannel<UnitResult>
        | GetState of AsyncReplyChannel<WorkerNodeMonitorResponse>


    type WorkerNodeRunner(i : WorkerNodeRunnerData) =
        let onRegisterProxy = onRegisterProxy i

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start (p, r) -> return! onStart p s |> (withReply r) |> loop
                            | Register r -> return! onRegister onRegisterProxy s |> (withReply r) |> loop
                            | Unregister r -> return! onUnregister onRegisterProxy s |> (withReply r) |> loop
                            | GetMessages (p, r) -> return! onGetMessages p s |> (withReply r) |> loop
                            | GetState r -> return! onGetState s |> (withReply r) |> loop
                        }

                WorkerNodeRunnerState.defaultValue |> loop
                )

        member w.start() = messageLoop.PostAndReply (fun reply -> Start (w.onStartProxy, reply))
        member _.register() = messageLoop.PostAndReply Register
        member _.unregister() = messageLoop.PostAndReply Unregister
        member w.getMessages() = messageLoop.PostAndReply (fun reply -> GetMessages (w.onGetMessagesProxy, reply))
        member _.getState() = messageLoop.PostAndReply GetState

        member _.onStartProxy =
            {
                loadAllActiveRunQueueId = i.workerNodeProxy.loadAllActiveRunQueueId
                onRunModel = i.workerNodeProxy.onProcessMessageProxy.onRunModel
            }

        member _.onGetMessagesProxy =
            {
                tryProcessMessage = onTryProcessMessage i.messageProcessorProxy
                onProcessMessage = fun w m -> w, onProcessMessage i.workerNodeProxy.onProcessMessageProxy m
                maxMessages = WorkerNodeRunnerState.maxMessages
                onError = fun f -> f |> OnGetMessagesErr |> WorkerNodeErr
                addError = fun a b -> a + b
            }


    let createServiceImpl (logger : Logger) (i : WorkerNodeRunnerData) =
        logger.logInfoString "createServiceImpl: Creating WorkerNodeRunner..."
        let w = WorkerNodeRunner i

        match i.workerNodeServiceInfo.workerNodeInfo.isInactive with
        | false ->
            logger.logInfoString "createServiceImpl: Registering..."
            match w.register >-> w.start |> evaluate with
            | Ok() ->
                let h = ClmEventHandler(ClmEventHandlerInfo.defaultValue logger w.getMessages "WorkerNodeRunner - getMessages")
                do h.start()

                // Attempt to restart solvers in case they did not start (due to whatever reason) or got killed.
                let s = ClmEventHandler(ClmEventHandlerInfo.oneHourValue logger w.start "WorkerNodeRunner - start")
                do s.start()

                Ok w
            | Error e -> Error e
        | true ->
            logger.logInfoString "createServiceImpl: Unregistering..."
            match w.unregister() with
            | Ok() -> failwith "createServiceImpl for inactive worker node is not implemented yet."
            | Error e -> Error e


    type WorkerNodeRunner
        with
        static member create (j : ClmResult<WorkerNodeServiceInfo>) =
            let logger = Logger.defaultValue
            let addError f e = ((f |> WorkerNodeServiceErr) + e) |> Error
            let c = getWorkerNodeSvcConnectionString

            let sr n (q : RunQueueId) : UnitResult =
                match tryRunSolverProcess n q with
                | Some _ -> Ok()
                | None -> q |> CannotRunModelErr |> OnRunModelErr |> WorkerNodeErr |> Error

            match j with
            | Ok i ->
                let w =
                    let messagingClientAccessInfo = i.messagingClientAccessInfo

                    let j =
                        {
                            messagingClientName = WorkerNodeServiceName.netTcpServiceName.value.value |> MessagingClientName
                            storageType = c |> MsSqlDatabase
                        }

                    let messagingClientData =
                        {
                            msgAccessInfo = messagingClientAccessInfo
                            communicationType = NetTcpCommunication
                            msgClientProxy = createMessagingClientProxy j messagingClientAccessInfo.msgClientId
                            expirationTime = MessagingClientData.defaultExpirationTime
                            logOnError = true
                        }

                    let messagingClient = MessagingClient messagingClientData

                    match messagingClient.start() with
                    | Ok() ->
                        let n =
                            {
                                workerNodeServiceInfo = i
                                workerNodeProxy = WorkerNodeProxy.create c (sr i.workerNodeInfo.noOfCores)
                                messageProcessorProxy = messagingClient.messageProcessorProxy
                            }
                            |> createServiceImpl logger

                        match n with
                        | Ok v -> Ok v
                        | Error e -> addError UnableToCreateWorkerNodeServiceErr e
                    | Error e -> addError UnableToStartMessagingClientErr e

                w
            | Error e -> Error e
