(*** hide ***)
#r @"..\..\bin\net40\FSharp.Data.SqlClient.dll"
#r "System.Transactions"
open FSharp.Data
open System

[<Literal>]
let connectionString = @"Data Source=.;Initial Catalog=AdventureWorks2014;Integrated Security=True"

(**

Bulk Load
===================

*)
