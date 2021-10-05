namespace SqlProviderTest

open FSharp.Data.Sql

module DatabaseTypes =

    [<Literal>]
    let ConnectionString = "Data Source=localhost;Initial Catalog=clm609;Integrated Security=SSPI"


    type private MyDatabase = SqlDataProvider<
                    Common.DatabaseProviderTypes.MSSQLSERVER_DYNAMIC,
                    ConnectionString = ConnectionString,
                    UseOptionTypes = true>


    type private MyContext = MyDatabase.dataContext


    let load() =
        let ctx = MyDatabase.GetDataContext()
        let x = ctx.Dbo.ClmDefaultValue
        0
