namespace MessagingService

open Softellect.Sys.Logging
open Softellect.Messaging.ServiceInfo
open Softellect.Messaging.Service
open Softellect.Sys.WcfErrors

open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open MessagingService.SvcCommandLine
open ServiceProxy.MsgServiceProxy
open DbData.Configuration
open Primitives.VersionInfo

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

                messagingServiceProxy = createMessagingServiceProxy getMessagingConnectionString
                communicationType = i.communicationType
            }

        let msgServiceDataRes = tryGetMsgServiceData i.messagingSvcInfo.messagingServiceAccessInfo logger serviceData
        msgServiceDataRes


    let messagingServiceData = Lazy<Result<MessagingWcfServiceData, WcfError>>(fun () -> tryCreateMessagingServiceData (Logger.defaultValue))
