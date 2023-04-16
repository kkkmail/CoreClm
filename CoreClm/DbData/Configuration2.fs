namespace DbData

open System
open System.Data.SQLite
open FSharp.Data.Sql

open Softellect.Sys.Retry
open Softellect.Sys.AppSettings

open ClmSys.VersionInfo
open ClmSys.GeneralPrimitives
open System.Data
open System.Data.SqlClient
open ClmSys.GeneralErrors
open ClmSys.ClmErrors
open ClmSys.GeneralData
open Primitives.GeneralData

module Configuration =

    [<Literal>]
    let ContGenDbName = ClmBaseName


    [<Literal>]
    let AppConfigFile : string = __SOURCE_DIRECTORY__ + @"\app.config"


    [<Literal>]
    let ContGenConnectionStringValue = "Server=localhost;Database=" + ContGenDbName + ";Integrated Security=SSPI"


    let contGenConnectionStringKey = ConfigKey "ContGenService"
    let messagingConnectionStringKey = ConfigKey "MessagingService"
    let workerNodeConnectionStringKey = ConfigKey "WorkerNodeService"


    let private getConnectionString fileName connKey defaultValue =
        match AppSettingsProvider.tryCreate fileName with
        | Ok provider ->
            match provider.tryGetConnectionString connKey with
            | Ok (Some EmptyString) -> defaultValue
            | Ok (Some s) -> s
            | _ -> defaultValue
        | _ -> defaultValue
        |> ConnectionString


    let private getContGenConnectionStringImpl() = getConnectionString appSettingsFile contGenConnectionStringKey ContGenConnectionStringValue


    let private contGenConnectionString = Lazy<ConnectionString>(getContGenConnectionStringImpl)
    let getContGenConnectionString() = contGenConnectionString.Value


    [<Literal>]
    let ClmCommandTimeout = 7200


    [<Literal>]
    let ContGenSqlProviderName : string = "name=ContGenService"


    [<Literal>]
    let MessagingDbName = MsgSvcBaseName


    [<Literal>]
    let MessagingConnectionStringValue = "Server=localhost;Database=" + MessagingDbName + ";Integrated Security=SSPI"


    let private getMessagingConnectionStringImpl() = getConnectionString appSettingsFile messagingConnectionStringKey MessagingConnectionStringValue
    let private messagingConnectionString = Lazy<ConnectionString>(getMessagingConnectionStringImpl)
    let getMessagingConnectionString() = messagingConnectionString.Value


    [<Literal>]
    let MessagingSqlProviderName : string = "name=MessagingService"


    [<Literal>]
    let WorkerNodeDbName = WorkerNodeSvcBaseName


    [<Literal>]
    let WorkerNodeConnectionStringValue = "Server=localhost;Database=" + WorkerNodeDbName + ";Integrated Security=SSPI"


    let private getWorkerNodeConnectionStringImpl() = getConnectionString appSettingsFile workerNodeConnectionStringKey WorkerNodeConnectionStringValue
    let private workerNodeConnectionString = Lazy<ConnectionString>(getWorkerNodeConnectionStringImpl)
    let getWorkerNodeSvcConnectionString() = workerNodeConnectionString.Value


    [<Literal>]
    let WorkerNodeSqlProviderName : string = "name=WorkerNodeService"


    let buildConnectionString (key : string) : string =
        [
            Some $"Server=localhost;Database=%s{key};Integrated Security=SSPI"
        ]
        |> List.pick (fun x -> x)


    let openConnIfClosed (conn : SqlConnection) =
        match conn.State with
        | ConnectionState.Closed -> do conn.Open()
        | _ -> ()


    let getOpenConn (c : unit -> ConnectionString) =
        let conn = new SqlConnection(c().value)
        openConnIfClosed conn
        conn


    let getOpenSqliteConn (SqliteConnectionString connectionString) =
        let conn = new SQLiteConnection(connectionString)
        conn.Open()
        conn


    let toError g f = f |> g |> DbErr |> Error
    let addError g f e = ((f |> g |> DbErr) + e) |> Error
    let mapException e = e |> DbExn |> DbErr
    let mapExceptionToError e = e |> DbExn |> DbErr |> Error


    /// Maps missing value (None) to DbErr.
    let mapDbError f i v =
        v
        |> Option.map Ok
        |> Option.defaultValue (i |> f |> DbErr |> Error)


    let tryDbFun g =
        let w() =
            try
                g()
            with
            | e -> mapExceptionToError e

        tryRopFun mapException w


    /// Analog of ExecuteScalar - gets the first column of the first result set.
    /// In contrast to ExecuteScalar it also expects it to be castable to int32.
    /// Otherwise it will return None.
    /// This function is monsly used to get the number of updated rows.
    let mapIntScalar (r : Common.SqlEntity[]) =
        r
        |> Array.map(fun e -> e.ColumnValues |> List.ofSeq |> List.head)
        |> Array.map snd
        |> Array.map (fun e -> match e with | :? Int32 as i -> Some i | _ -> None)
        |> Array.tryHead
        |> Option.bind id


    /// Binds an unsuccessful database update operation to a given continuation function.
    let bindError f q r =
        match r = 1 with
        | true -> Ok ()
        | false -> toError f q


    /// Binds an unsuccessful database update operation to a given continuation function.
    let bindOptionError f q r =
        match r = (Some 1) with
        | true -> Ok ()
        | false -> toError f q


    let bindIntScalar  f q r = r |> mapIntScalar |> bindOptionError f q
