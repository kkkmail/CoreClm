namespace DbData

open System
open System.Data.SQLite
open Softellect.Sys.Retry
open Softellect.Sys.AppSettings

open ClmSys.VersionInfo
open ClmSys.GeneralPrimitives
open System.Data
open System.Data.SqlClient
open ClmSys.GeneralErrors
open ClmSys.ClmErrors
open ClmSys.GeneralData

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
            Some (sprintf "Server=localhost;Database=%s;Integrated Security=SSPI" key)
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
