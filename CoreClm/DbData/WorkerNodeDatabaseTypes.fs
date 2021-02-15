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

module WorkerNodeDatabaseTypes =

    type WorkerNodeDB = SqlProgrammabilityProvider<WorkerNodeSqlProviderName, ConfigFile = WorkerNodeAppConfigFile>
    type RunQueueTable = WorkerNodeDB.dbo.Tables.RunQueue
    type RunQueueTableRow = RunQueueTable.Row
