namespace ServiceProxy

open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.Logging
open Softellect.Messaging.Primitives
open Softellect.Messaging.Proxy

open MessagingServiceInfo.ServiceInfo
open ClmSys.ClmErrors
open DbData.MsgSvcDatabaseTypes
open ClmSys.GeneralPrimitives

module MsgServiceProxy =

    type MessagingClientStorageType =
        | MsSqlDatabase of (unit -> ConnectionString)
        | SqliteDatabase of SqliteConnectionString


    type MessagingClientProxyInfo =
        {
            messagingClientName : MessagingClientName
            storageType : MessagingClientStorageType
        }


    let createMessagingClientProxy (i : MessagingClientProxyInfo) (c : MessagingClientId) =
        let getMessageSize (e : MessageData<ClmMessageData>) =
            match e with
            | SystemMsg _ -> SmallSize
            | UserMsg m -> m.getMessageSize()

        match i.storageType with
        | MsSqlDatabase g ->
            {
                tryPickIncomingMessage = fun () -> tryPickIncomingMessage g c
                tryPickOutgoingMessage = fun () -> tryPickOutgoingMessage g c
                saveMessage = saveMessage g
                tryDeleteMessage = deleteMessage g
                deleteExpiredMessages = deleteExpiredMessages g
                getMessageSize = getMessageSize
                logger = Logger.defaultValue
                toErr = fun e -> e |> MessagingClientErr
                addError = fun a b -> a + b
            }
        | SqliteDatabase connectionString ->
            {
                tryPickIncomingMessage = fun () -> tryPickIncomingMessageSqlite connectionString c
                tryPickOutgoingMessage = fun () -> tryPickOutgoingMessageSqlite connectionString c
                saveMessage = saveMessageSqlite connectionString
                tryDeleteMessage = deleteMessageSqlite connectionString
                deleteExpiredMessages = deleteExpiredMessagesSqlite connectionString
                getMessageSize = getMessageSize
                logger = Logger.defaultValue
                toErr = fun e -> e |> MessagingClientErr
                addError = fun a b -> a + b
            }


    let createMessagingServiceProxy (g : unit -> ConnectionString) =
        {
            tryPickMessage = tryPickIncomingMessage g
            saveMessage = saveMessage g
            deleteMessage = deleteMessage g
            deleteExpiredMessages = deleteExpiredMessages g
            logger = Logger.defaultValue
            toErr = fun e -> e |> MessagingServiceErr
        }
