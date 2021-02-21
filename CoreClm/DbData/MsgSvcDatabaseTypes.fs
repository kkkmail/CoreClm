namespace DbData

open FSharp.Data
open System
open FSharp.Data.Sql
open System.Data.SQLite
open Dapper
open System.Data.Common

open Softellect.Sys.Primitives
open Softellect.Sys.Core
open Softellect.Sys.MessagingPrimitives
open Softellect.Messaging.Primitives
open Softellect.Sys.MessagingClientErrors
open Softellect.Sys.MessagingServiceErrors

open ClmSys.VersionInfo
open ClmSys.ClmErrors
open ClmSys.GeneralPrimitives
open Clm.ModelParams
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo

// ! Must be the last to open !
open Configuration

module MsgSvcDatabaseTypes =

    [<Literal>]
    let private SqliteStorageFolder = DefaultRootFolder


    [<Literal>]
    let MsgSqliteConnStr =
        "Data Source=" + __SOURCE_DIRECTORY__ + @"\" + MsgDatabase + @";Version=3;foreign keys=true"

    let getSqlLiteConnStr msgDbLocation = @"Data Source=" + msgDbLocation + ";Version=3;foreign keys=true"
    let msgSqliteConnStr = MsgSqliteConnStr |> SqliteConnectionString


    type Guid
        with
        member g.ToSqliteString() = g.ToString("N")


//    type sqLite = SqlDataProvider<
//                   Common.DatabaseProviderTypes.SQLITE,
//                   SQLiteLibrary = Common.SQLiteLibrary.SystemDataSQLite,
//                   ConnectionString = MsgSqliteConnStr,
//                   //ResolutionPath = resolutionPath,
//                   CaseSensitivityChange = Common.CaseSensitivityChange.ORIGINAL>



    let serializationFormat = BinaryZippedFormat


    type MsgSvcDB = SqlProgrammabilityProvider<MessagingSqlProviderName, ConfigFile = AppConfigFile>
    type MessageTable = MsgSvcDB.dbo.Tables.Message
    type MessageTableRow = MessageTable.Row


    type MessageDatabaseData = SqlCommandProvider<"
        select *
        from dbo.Message
        where messageId = @messageId", MessagingConnectionStringValue, ResultType.DataReader>


    type TryPickRecipientMessageData = SqlCommandProvider<"
           select top 1 *
           from dbo.Message
           where recipientId = @recipientId and dataVersion = @dataVersion
           order by messageOrder
           ", MessagingConnectionStringValue, ResultType.DataReader>


    type TryPickSenderMessageData = SqlCommandProvider<"
           select top 1 *
           from dbo.Message
           where senderId = @senderId and dataVersion = @dataVersion
           order by messageOrder
           ", MessagingConnectionStringValue, ResultType.DataReader>


    let tryCreateMessageImpl (r : MessageTableRow) =
            let toError e = e |> MessageCreateErr |> MsgSvcDbErr |> MessagingServiceErr |> Error

            let g() =
                match MessageDeliveryType.tryCreate r.deliveryTypeId, messagingDataVersion.value = r.dataVersion with
                | Some t, true ->
                    {
                        messageDataInfo =
                            {
                                messageId = r.messageId |> MessageId
                                dataVersion = r.dataVersion |> MessagingDataVersion
                                sender = r.senderId |> MessagingClientId

                                recipientInfo =
                                    {
                                        recipient = r.recipientId |> MessagingClientId
                                        deliveryType = t
                                    }

                                createdOn = r.createdOn
                            }

                        messageData = r.messageData |> deserialize serializationFormat
                    }
                    |> Some
                    |> Ok
                | Some _, false -> InvalidDataVersionErr { localVersion = messagingDataVersion; remoteVersion = MessagingDataVersion r.dataVersion } |> toError
                | None, true -> InvalidDeliveryTypeErr r.deliveryTypeId |> toError
                | None, false -> InvalidDeliveryTypeAndDataVersionErr (r.deliveryTypeId, { localVersion = messagingDataVersion; remoteVersion = MessagingDataVersion r.dataVersion }) |> toError

            tryDbFun g


    let addMessageTableRow (r : Message) (t : MessageTable) =
            let g() =
                let newRow =
                    t.NewRow(
                            messageId = r.messageDataInfo.messageId.value,
                            dataVersion = messagingDataVersion.value,
                            deliveryTypeId = r.messageDataInfo.recipientInfo.deliveryType.value,
                            senderId = r.messageDataInfo.sender.value,
                            recipientId = r.messageDataInfo.recipientInfo.recipient.value,
                            messageData = (r.messageData |> serialize serializationFormat)
                            )

                newRow.createdOn <- DateTime.Now // Set our local time as we don't care about remote time.

                t.Rows.Add newRow
                Ok newRow

            tryDbFun g


    let tryCreateMessage (t : MessageTable) =
        match t.Rows |> Seq.tryHead with
        | Some v -> v |> tryCreateMessageImpl
        | None -> Ok None


    let tryPickIncomingMessage c (MessagingClientId i) =
        let g () =
            use conn = getOpenConn c
            let t = new MessageTable()
            use d = new TryPickRecipientMessageData(conn)
            d.Execute(i, messagingDataVersion.value) |> t.Load
            tryCreateMessage t

        tryDbFun g


    let tryPickOutgoingMessage c (MessagingClientId i) =
        let g () =
            use conn = getOpenConn c
            let t = new MessageTable()
            use d = new TryPickSenderMessageData(conn)
            d.Execute(i, messagingDataVersion.value) |> t.Load
            tryCreateMessage t

        tryDbFun g


    /// We consider the messages are write once, so if the message is already in the database, then we just ignore it.
    ///
    /// Using "with (holdlock)" seems to be causing some deadlocks.
    ///                merge Message with (holdlock) as target
    ///                using (select @messageId, @senderId, @recipientId, @dataVersion, @deliveryTypeId, @messageData, @createdOn)
    ///                as source (messageId, senderId, recipientId, dataVersion, deliveryTypeId, messageData, createdOn)
    ///                on (target.messageId = source.messageId)
    ///                when not matched then
    ///                    insert (messageId, senderId, recipientId, dataVersion, deliveryTypeId, messageData, createdOn)
    ///                    values (source.messageId, source.senderId, source.recipientId, source.dataVersion, source.deliveryTypeId, source.messageData, source.createdOn)
    ///                when matched then
    ///                    update set senderId = source.senderId, recipientId = source.recipientId, dataVersion = source.dataVersion, deliveryTypeId = source.deliveryTypeId, messageData = source.messageData, createdOn = source.createdOn;
    let saveMessage c (m : Message) =
//        let toError e = e |> MessageCreateErr |> MsgSvcDbErr |> MessagingServiceErr |> Error

        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString

            use cmd = new SqlCommandProvider<"
                declare @messageIdValue uniqueidentifier
                set @messageIdValue = @messageId

				insert into Message (messageId, senderId, recipientId, dataVersion, deliveryTypeId, messageData, createdOn)
				select @messageIdValue, @senderId, @recipientId, @dataVersion, @deliveryTypeId, @messageData, @createdOn
				where not exists (select 1 from Message where messageId = @messageIdValue)
            ", MessagingConnectionStringValue>(connectionString, commandTimeout = ClmCommandTimeout)

            let result = cmd.Execute(messageId = m.messageDataInfo.messageId.value
                                    ,senderId = m.messageDataInfo.sender.value
                                    ,recipientId = m.messageDataInfo.recipientInfo.recipient.value
                                    ,dataVersion = messagingDataVersion.value
                                    ,deliveryTypeId = m.messageDataInfo.recipientInfo.deliveryType.value
                                    ,messageData = (m.messageData |> serialize serializationFormat)
                                    ,createdOn = DateTime.Now) // Set our local time as we don't care about remote time.

            match result with
            | 1 -> Ok ()
            | _ -> m.messageDataInfo.messageId |> CannotUpsertMessageErr |> MessageUpsertErr |> MessagingServiceErr |> Error

        tryDbFun g


    let deleteMessage c (messageId : MessageId) =
        let toError e = e |> MessageDeleteErr |> MsgSvcDbErr |> MessagingServiceErr |> Error

        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString

            use cmd = new SqlCommandProvider<"delete from dbo.Message where messageId = @messageId", MessagingConnectionStringValue>(connectionString)

            match cmd.Execute(messageId = messageId.value) with
            | 0 | 1 -> Ok()
            | _ -> messageId |> MessageDeleteError.CannotDeleteMessageErr |> toError

        tryDbFun g


    let deleteExpiredMessages c (expirationTime : TimeSpan) =
        let g() =
            use conn = getOpenConn c
            let connectionString = conn.ConnectionString

            use cmd = new SqlCommandProvider<"
                delete from dbo.Message
                where
                    deliveryTypeId = 1
                    and dataVersion = @dataVersion
                    and createdOn < @createdOn", MessagingConnectionStringValue>(connectionString)

            let _ = cmd.Execute(messagingDataVersion.value, DateTime.Now - expirationTime)
            Ok()

        tryDbFun g


    let private executeSqlite (connection : #DbConnection) (sql : string) (parameters : _) =
        let g() =
            let result = connection.Execute(sql, parameters)
            Ok result
        tryDbFun g


    /// TODO kk:20200523 - So far this looks extremely far beyond ugly.
    /// Find the proper way and don't go beyond this one table until that proper way is found.
    ///
    /// Here are some references:
    ///     https://devonburriss.me/how-to-fsharp-pt-9/
    ///     https://isthisit.nz/posts/2019/sqlite-database-with-dapper-and-fsharp/
    ///     http://zetcode.com/csharp/sqlite/
    let saveMessageSqlite (SqliteConnectionString connectionString) (m : Message) =
        let g() =
            use connectionString = new SQLiteConnection(connectionString)

            let sql = @"
                insert into Message
                    (messageId
                    ,senderId
                    ,recipientId
                    ,dataVersion
                    ,deliveryTypeId
                    ,messageData
                    ,createdOn)
                values
                    (@messageId
                    ,@senderId
                    ,@recipientId
                    ,@dataVersion
                    ,@deliveryTypeId
                    ,@messageData
                    ,@createdOn)"

            let data =
                [
                    ("@messageId", m.messageDataInfo.messageId.value.ToSqliteString() |> box)
                    ("@senderId", m.messageDataInfo.sender.value.ToSqliteString() |> box)
                    ("@recipientId", m.messageDataInfo.recipientInfo.recipient.value.ToSqliteString() |> box)
                    ("@dataVersion", m.messageDataInfo.dataVersion.value |> box)
                    ("@deliveryTypeId", m.messageDataInfo.recipientInfo.deliveryType.value |> box)
                    ("@messageData", m.messageData |> (serialize serializationFormat) |> box)
                    ("@createdOn", m.messageDataInfo.createdOn |> box)
                ]
                |> dict
                |> fun d -> DynamicParameters(d)

            let _ = executeSqlite connectionString sql data
            Ok()

        tryDbFun g


    let deleteMessageSqlite connectionString (messageId : MessageId) =
        let toError e = e |> SendMessageErr |> MessagingClientErr |> Error

        let g() =
            use conn = getOpenSqliteConn connectionString
            use cmd = new SQLiteCommand("delete from Message where messageId = @messageId", conn)
            cmd.Parameters.Add(SQLiteParameter("@messageId", messageId.value.ToSqliteString())) |> ignore

            match cmd.ExecuteNonQuery() with
            | 0 | 1 -> Ok()
            | _ -> messageId |> SendMessageError.CannotDeleteMessageErr |> toError

        tryDbFun g


    let deleteExpiredMessagesSqlite connectionString (expirationTime : TimeSpan) =
        let g() =
            use conn = getOpenSqliteConn connectionString
            use cmd = new SQLiteCommand(@"
                delete from Message
                where
                    deliveryTypeId = 1
                    and dataVersion = @dataVersion
                    and createdOn < @createdOn", conn)

            cmd.Parameters.Add(SQLiteParameter("@dataVersion", messagingDataVersion.value)) |> ignore
            cmd.Parameters.Add(SQLiteParameter("@createdOn", DateTime.Now - expirationTime)) |> ignore

            let _ = cmd.ExecuteNonQuery()
            Ok()

        tryDbFun g


    type SQLiteDataReader
        with
        member rdr.GetGuid(columnName : string) = rdr.GetString(rdr.GetOrdinal(columnName)) |> Guid.Parse
        member rdr.GetInt16(columnName : string) = rdr.GetInt16(rdr.GetOrdinal(columnName))
        member rdr.GetInt32(columnName : string) = rdr.GetInt32(rdr.GetOrdinal(columnName))
        member rdr.GetInt64(columnName : string) = rdr.GetInt64(rdr.GetOrdinal(columnName))
        member rdr.GetDateTime(columnName : string) = rdr.GetDateTime(rdr.GetOrdinal(columnName))
        member rdr.GetBoolean(columnName : string) = rdr.GetBoolean(rdr.GetOrdinal(columnName))

        member rdr.GetBlob(columnName : string) =
            let len = rdr.GetBytes(rdr.GetOrdinal(columnName), 0L, null, 0, Int32.MaxValue) |> int
            let bytes : byte array = Array.zeroCreate len
            rdr.GetBytes(rdr.GetOrdinal(columnName), 0L, bytes, 0, bytes.Length) |> ignore
            bytes


    let toMessage (rdr : SQLiteDataReader) =
        {
            messageDataInfo =
                {
                    messageId = rdr.GetGuid("messageId") |> MessageId
                    dataVersion = rdr.GetInt32("dataVersion") |> MessagingDataVersion
                    sender = rdr.GetGuid("senderId") |> MessagingClientId
                    recipientInfo =
                        {
                            recipient = rdr.GetGuid("recipientId") |> MessagingClientId
                            deliveryType = rdr.GetInt32("deliveryTypeId")
                                           |> MessageDeliveryType.tryCreate
                                           |> Option.defaultValue GuaranteedDelivery
                        }

                    createdOn = rdr.GetDateTime("createdOn")
                }

            messageData = rdr.GetBlob("messageData") |> (deserialize serializationFormat)
        }


    let tryPickIncomingMessageSqlite connectionString (MessagingClientId i) =
        let g () =
            use conn = getOpenSqliteConn connectionString
            use cmd = new SQLiteCommand(@"
                select *
                from Message
                where recipientId = @recipientId and dataVersion = @dataVersion
                order by messageOrder
                limit 1", conn)

            cmd.Parameters.Add(SQLiteParameter("@recipientId", i.ToSqliteString())) |> ignore
            cmd.Parameters.Add(SQLiteParameter("@dataVersion", messagingDataVersion.value)) |> ignore
            use rdr = cmd.ExecuteReader()

            match rdr.Read() with
            | true -> toMessage rdr |> Some
            | false -> None
            |> Ok

        tryDbFun g


    let tryPickOutgoingMessageSqlite connectionString (MessagingClientId i) =
        let g () =
            use conn = getOpenSqliteConn connectionString
            use cmd = new SQLiteCommand(@"
                select *
                from Message
                where senderId = @senderId and dataVersion = @dataVersion
                order by messageOrder
                limit 1", conn)

            cmd.Parameters.Add(SQLiteParameter("@senderId", i.ToSqliteString())) |> ignore
            cmd.Parameters.Add(SQLiteParameter("@dataVersion", messagingDataVersion.value)) |> ignore
            use rdr = cmd.ExecuteReader()

            match rdr.Read() with
            | true -> toMessage rdr |> Some
            | false -> None
            |> Ok

        tryDbFun g
