namespace DbData

open System
open System.Data
open System.Data.SqlClient

module DynamicSql =

    /// http://tomasp.net/blog/dynamic-sql.aspx/
    /// Wrappers with dynamic operators for creating SQL Store Procedure calls.
    type DynamicSqlDataReader(reader:SqlDataReader) =
        member private _.Reader = reader
        member _.Read() = reader.Read()

        /// http://www.fssnip.net/hh/title/Dynamic-operator-with-null-handling
        static member (?) (dr:DynamicSqlDataReader, name:string) : 'R =
            let typ = typeof<'R>
            if typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<option<_>> then
                if dr.Reader.[name] = box DBNull.Value then
                    (box null) :?> 'R
                else typ.GetMethod("Some").Invoke(null, [| dr.Reader.[name] |]) :?> 'R
            else dr.Reader.[name] :?> 'R

        interface IDisposable with
            member x.Dispose() = reader.Dispose()


    type DynamicSqlCommand(cmd:SqlCommand) =
        member private _.Command = cmd

        static member (?<-) (cmd:DynamicSqlCommand, name:string, value) =
            cmd.Command.Parameters.Add(SqlParameter("@" + name, box value)) |> ignore

        member _.ExecuteNonQuery() = cmd.ExecuteNonQuery()
        member _.ExecuteReader() = new DynamicSqlDataReader(cmd.ExecuteReader())
        member _.ExecuteScalar() = cmd.ExecuteScalar()
        member _.Parameters = cmd.Parameters

        interface IDisposable with
            member _.Dispose() = cmd.Dispose()


    type DynamicSqlConnection(conn:SqlConnection) =
        member private _.Connection = conn

        static member (?) (conn:DynamicSqlConnection, name) =
            let command = new SqlCommand(name, conn.Connection)
            command.CommandType <- CommandType.StoredProcedure
            new DynamicSqlCommand(command)

        member _.Open() = conn.Open()
        new (connStr:string) = new DynamicSqlConnection(connStr)

        interface IDisposable with
            member _.Dispose() = conn.Dispose()
