namespace SqlProviderTest

open FSharp.Data.Sql

module Say =

    [<Literal>]
    let ConnectionString = "Data Source=localhost;Initial Catalog=clm608;Integrated Security=SSPI"


    type sql = SqlDataProvider<Common.DatabaseProviderTypes.MSSQLSERVER, ConnectionString>


    let load() =
        let ctx = sql.GetDataContext()


        0
