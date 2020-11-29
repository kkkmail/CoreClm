namespace ServiceProxy

open MessagingServiceInfo.ServiceInfo
open ClmSys.MessagingPrimitives
open ClmSys.ClmErrors
open DbData.MsgSvcDatabaseTypes
open System
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


    /// Provides IO proxy for messaging client.
    /// Currently it is assumed that messaging client may NOT have SQL server at its disposal.
    type MessagingClientProxy =
        {
            tryPickIncomingMessage : unit -> ClmResult<Message option>
            tryPickOutgoingMessage : unit -> ClmResult<Message option>
            saveMessage : Message -> UnitResult
            tryDeleteMessage : MessageId -> UnitResult
            deleteExpiredMessages : TimeSpan -> UnitResult
        }

        static member create (i : MessagingClientProxyInfo) (c : MessagingClientId) =
            let name = i.messagingClientName

            match i.storageType with
            | MsSqlDatabase g ->

                {
                    tryPickIncomingMessage = fun () -> tryPickIncomingMessage g c
                    tryPickOutgoingMessage = fun () -> tryPickOutgoingMessage g c
                    saveMessage = fun m -> saveMessage g m
                    tryDeleteMessage = deleteMessage g
                    deleteExpiredMessages = deleteExpiredMessages g
                }
            | SqliteDatabase connectionString ->
                {
                    tryPickIncomingMessage = fun () -> tryPickIncomingMessageSqlite connectionString c
                    tryPickOutgoingMessage = fun () -> tryPickOutgoingMessageSqlite connectionString c
                    saveMessage = fun m -> saveMessageSqlite connectionString m
                    tryDeleteMessage = deleteMessageSqlite connectionString
                    deleteExpiredMessages = deleteExpiredMessagesSqlite connectionString
                }


    /// Provides IO proxy for messaging service.
    type MessagingServiceProxy =
        {
            tryPickMessage : MessagingClientId -> ClmResult<Message option>
            saveMessage : Message -> UnitResult
            deleteMessage : MessageId -> UnitResult
            deleteExpiredMessages : TimeSpan -> UnitResult
        }

        static member create (g : unit -> ConnectionString) =
            {
                tryPickMessage = tryPickIncomingMessage g
                saveMessage = saveMessage g
                deleteMessage = deleteMessage g
                deleteExpiredMessages = deleteExpiredMessages g
            }
