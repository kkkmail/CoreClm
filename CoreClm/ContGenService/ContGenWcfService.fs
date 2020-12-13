namespace ContGenService

open System
open Argu

open Softellect.Sys.Primitives
open Softellect.Sys.Core
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.MessagingServiceErrors
open Softellect.Messaging.ServiceInfo
open Softellect.Sys.Core
open Softellect.Sys.Primitives
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.Logging
open Softellect.Sys.MessagingErrors
open Softellect.Wcf.Common
open Softellect.Wcf.Service
open Softellect.Sys.WcfErrors
open Softellect.Messaging.Primitives
open Softellect.Messaging.ServiceInfo
open Softellect.Messaging.Service
open Softellect.Messaging.Client
open Softellect.Messaging.Proxy
open Softellect.Sys.MessagingClientErrors
open Softellect.Sys.MessagingServiceErrors

open ClmSys.GeneralData
open ClmSys.ClmWorker
open ClmSys.VersionInfo
open ClmSys.Logging
open Messaging.ServiceResponse
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
open Messaging.Client
open ClmSys.GeneralPrimitives
open ClmSys.ContGenPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.PartitionerPrimitives
open ContGen.ModelRunner
open ClmSys.MessagingData
open ClmSys.ContGenData
open Clm.ModelParams
open DbData.Configuration
open ContGenServiceInfo.ServiceInfo

module ContGenWcfService =

    type ContGenService() =

        interface IContGenService with
            member _.tryCancelRunQueue r c = failwith "123456"
            member _.tryRequestResults r n = failwith "123456"

        //abstract tryCancelRunQueue : RunQueueId -> CancellationType -> UnitResult
        //abstract tryRequestResults : RunQueueId -> ResultNotificationType -> UnitResult


    type ContGenWcfService() =
        let service = ContGenService() :> IContGenService

        interface IContGenWcfService with
            member _.tryCancelRunQueue q = failwith "123456"
            member _.tryRequestResults q = failwith "123456"


    type ContGenWcfServiceImpl = WcfService<ContGenWcfService, IContGenWcfService, ContGenServiceData>


    let contGenServiceData = Lazy<Result<MessagingWcfServiceData, WcfError>>(fun () -> tryCreateMessagingServiceData (Logger.defaultValue))
