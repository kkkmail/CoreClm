namespace ServiceProxy

open MessagingServiceInfo.ServiceInfo
open ClmSys.ClmErrors
open ClmSys.MessagingClientErrors
open ClmSys.MessagingPrimitives

module MsgProcessorProxy =

    type MessageProcessorResult<'T> =
        | ProcessedSuccessfully of 'T
        | ProcessedWithError of ('T * ClmError)
        | ProcessedWithFailedToRemove of ('T * ClmError)
        | FailedToProcess of ClmError
        | NothingToDo
        | BusyProcessing


    type MessageProcessorProxy =
        {
            start : unit -> UnitResult
            tryPeekReceivedMessage : unit -> ClmResult<Message option>
            tryRemoveReceivedMessage : MessageId -> UnitResult
            sendMessage : MessageInfo -> UnitResult
            tryReceiveMessages : unit -> UnitResult
            trySendMessages : unit -> UnitResult
            removeExpiredMessages : unit -> UnitResult
        }


    type OnProcessMessageType<'S> = 'S -> Message -> StateWithResult<'S>
    type MessageResult<'S> = MessageProcessorResult<'S * UnitResult>


    type OnGetMessagesProxy<'S> =
        {
            tryProcessMessage : 'S -> OnProcessMessageType<'S> -> MessageResult<'S>
            onProcessMessage : 'S -> Message -> StateWithResult<'S>
            maxMessages : list<unit>
            onError : OnGetMessagesError -> ClmError
        }


    let onGetMessages<'S> (proxy : OnGetMessagesProxy<'S>) (s : 'S) =
        //printfn "onGetMessages: Getting messages..."
        let addError f e = ((proxy.onError f) + e) |> Error
        let toError e = e |> proxy.onError |> Error

        let rec doFold x (acc, r) =
            match x with
            | [] -> acc, Ok()
            | () :: t ->
                //printfn "onGetMessages: Calling proxy.tryProcessMessage..."
                match proxy.tryProcessMessage acc proxy.onProcessMessage with
                | ProcessedSuccessfully (g, u) ->
                    match u with
                    | Ok() -> doFold t (g, r)
                    | Error e ->
                        printfn "onGetMessages: Got error: %A" e
                        doFold t (g, (addError ProcessedSuccessfullyWithInnerErr e, r) ||> combineUnitResults)
                | ProcessedWithError ((g, u), e) -> g, [ addError ProcessedWithErr e; u; r ] |> foldUnitResults
                | ProcessedWithFailedToRemove((g, u), e) -> g, [ addError ProcessedWithFailedToRemoveErr e; u; r ] |> foldUnitResults
                | FailedToProcess e -> acc, addError FailedToProcessErr e
                | NothingToDo -> acc, Ok()
                | BusyProcessing -> acc, toError BusyProcessingErr

        let w, result = doFold proxy.maxMessages (s, Ok())
        //printfn "onGetMessages: result = %A" result
        w, result
