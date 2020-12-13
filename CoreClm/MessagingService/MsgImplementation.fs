namespace MessagingService

open Softellect.Sys.Primitives
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.Logging
open Softellect.Sys.MessagingErrors
open Softellect.Wcf.Common
open Softellect.Wcf.Service
open Softellect.Messaging.Primitives
open Softellect.Messaging.ServiceInfo
open Softellect.Messaging.Service
open Softellect.Sys.WcfErrors
open Softellect.Messaging.Client
open Softellect.Messaging.Proxy

open System.ServiceModel
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open MessagingService.SvcCommandLine
open Messaging.Service
open ServiceProxy.MsgServiceProxy
open ClmSys.ClmErrors
open DbData.Configuration
open ClmSys.VersionInfo

module ServiceImplementation =

    let mutable serviceSettings = getServiceSettings []


    let tryCreateMessagingServiceData logger : Result<MessagingWcfServiceData, WcfError> =
        let i = getServiceSettings []

        let serviceData : MessagingServiceData =
            {
                messagingServiceInfo =
                    {
                        expirationTime = i.messagingInfo.expirationTime
                        messagingDataVersion = messagingDataVersion
                    }

                messagingServiceProxy = createMessagingServiceProxy getMsgSvcConnectionString
                communicationType = i.communicationType
            }

        let msgServiceDataRes = tryGetMsgServiceData i.messagingSvcInfo.messagingServiceAccessInfo logger serviceData
        msgServiceDataRes


    let messagingServiceData = Lazy<Result<MessagingWcfServiceData, WcfError>>(fun () -> tryCreateMessagingServiceData (Logger.defaultValue))
