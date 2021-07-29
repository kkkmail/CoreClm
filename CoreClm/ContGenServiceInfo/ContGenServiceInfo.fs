namespace ContGenServiceInfo

open System
open System.ServiceModel
open System.Threading

open ClmSys.ContGenPrimitives
open ClmSys.DistributionData
open ClmSys.SolverData
open Softellect.Sys
open Softellect.Sys.MessagingPrimitives
open Softellect.Sys.AppSettings
open Softellect.Wcf.Common
open Softellect.Sys.Primitives
open Softellect.Messaging.ServiceInfo

open ClmSys.VersionInfo
open ClmSys.MessagingPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.GeneralData
open ClmSys.GeneralPrimitives
open ClmSys.SolverRunnerPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.ClmErrors
open ClmSys.ContGenErrors
open ClmSys.ContGenData
open ClmSys.PartitionerData
open ClmSys.ModelData
open Clm.ModelParams

module ServiceInfo =

    let contGenServiceProgramName = "ContGenService.exe"


    [<Literal>]
    let ContGenWcfServiceName = "ContGenWcfService"


    type RunningProcessData =
        {
            modelDataId : ModelDataId
            defaultValueId : ClmDefaultValueId
            runQueueId : RunQueueId
            workerNodeId : WorkerNodeId
            commandLineParams : ModelCommandLineParam
        }


    type ProgressUpdateInfo =
        {
            runQueueId : RunQueueId
            updatedRunQueueStatus : RunQueueStatus option
            progressData : ProgressData
        }


    type RunningProcessInfo =
        {
            started : DateTime
            progressUpdateInfo : ProgressUpdateInfo
        }


    type ProgressUpdateInfo
        with
        member this.toRunningProcessInfo() =
            {
                started = DateTime.Now
                progressUpdateInfo = this
            }


    let mutable private callCount = -1


    let getServiceState (getState : unit -> list<RunQueue> * UnitResult) =
        if Interlocked.Increment(&callCount) = 0
        then
            try
                printfn "Getting state at %s ..." (DateTime.Now.ToString("yyyy-MM-dd.HH:mm:ss"))
                let q, _ = getState()
                let r0 = q |> List.sortBy (fun e -> e.progressData.progress) |> List.map (fun e -> "      " + e.ToString()) |> String.concat Nl
                let r = if r0 = EmptyString then "[]" else Nl + "    [" + Nl + r0 + Nl + "    ]"
                printfn "... state at %s\n{\n  running = %s\n  runningCount = %A\n }"  (DateTime.Now.ToString("yyyy-MM-dd.HH:mm:ss")) r q.Length
            with
            | e -> printfn $"Exception occurred: %A{e}"
        else
            printfn $"Not getting state at %A{DateTime.Now} because callCount = %A{callCount}."

        Interlocked.Decrement(&callCount) |> ignore
        Ok()


    type IContGenService =
        abstract tryCancelRunQueue : RunQueueId -> CancellationType -> UnitResult
        abstract tryRequestResults : RunQueueId -> ResultNotificationType -> UnitResult
        abstract tryReset : RunQueueId -> UnitResult


    /// https://gist.github.com/dgfitch/661656
    [<ServiceContract(ConfigurationName = ContGenWcfServiceName)>]
    type IContGenWcfService =

        [<OperationContract(Name = "tryCancelRunQueue")>]
        abstract tryCancelRunQueue : q:byte[] -> byte[]

        [<OperationContract(Name = "tryRequestResults")>]
        abstract tryRequestResults : q:byte[] -> byte[]

        [<OperationContract(Name = "tryReset")>]
        abstract tryReset : q:byte[] -> byte[]


    let contGenServiceAddress = ConfigKey "ContGenServiceAddress"
    let contGenServiceHttpPort = ConfigKey "ContGenServiceHttpPort"
    let contGenServiceNetTcpPort = ConfigKey "ContGenServiceNetTcpPort"
    let contGenServiceCommunicationType = ConfigKey "ContGenServiceCommunicationType"

    let messagingServiceAddress = ConfigKey "MessagingServiceAddress"
    let messagingHttpServicePort = ConfigKey "MessagingHttpServicePort"
    let messagingNetTcpServicePort = ConfigKey "MessagingNetTcpServicePort"
    let messagingServiceCommunicationType = ConfigKey "MessagingServiceCommunicationType"

    let minUsefulEe = ConfigKey "MinUsefulEe"
    let partitionerId = ConfigKey "PartitionerId"
    let lastAllowedNodeErrInMinutes = ConfigKey "LastAllowedNodeErrInMinutes"
    let earlyExitCheckFrequencyInMinutes = ConfigKey "EarlyExitCheckFrequencyInMinutes"
    let dictionaryUpdateType = ConfigKey "DictionaryUpdateType"
    let absoluteTolerance = ConfigKey "AbsoluteTolerance"


    let updateContGenSettings (provider : AppSettingsProvider) (c : ContGenServiceAccessInfo) (ct : WcfCommunicationType)  =
        let h = c.value.httpServiceInfo
        let n = c.value.netTcpServiceInfo

        provider.trySet contGenServiceAddress n.netTcpServiceAddress.value |> ignore
        provider.trySet contGenServiceHttpPort h.httpServicePort.value |> ignore
        provider.trySet contGenServiceNetTcpPort n.netTcpServicePort.value |> ignore
        provider.trySet contGenServiceCommunicationType ct.value |> ignore


    let updateMessagingSettings (provider : AppSettingsProvider) (m : MessagingServiceAccessInfo) (ct : WcfCommunicationType)  =
        let mh = m.messagingServiceAccessInfo.httpServiceInfo
        let mn = m.messagingServiceAccessInfo.netTcpServiceInfo

        provider.trySet messagingServiceAddress mn.netTcpServiceAddress.value |> ignore
        provider.trySet messagingHttpServicePort mh.httpServicePort.value |> ignore
        provider.trySet messagingNetTcpServicePort mn.netTcpServicePort.value |> ignore
        provider.trySet messagingServiceCommunicationType ct.value |> ignore

    let sugSynthCollKey = ConfigKey "SugSynthColl"
    let catSynthCollKey = ConfigKey "CatSynthColl"
    let enCatSynthCollKey = ConfigKey "EnCatSynthColl"
    let acCatSynthCollKey = ConfigKey "AcCatSynthColl"
    let catDestrCollKey = ConfigKey "CatDestrColl"
    let enCatDestrCollKey = ConfigKey "EnCatDestrColl"
    let acCatDestrCollKey = ConfigKey "AcCatDestrColl"
    let catLigCollKey = ConfigKey "CatLigColl"
    let enCatLigCollKey = ConfigKey "EnCatLigColl"
    let acFwdCatLigCollKey = ConfigKey "AcFwdCatLigColl"
    let acBkwCatLigCollKey = ConfigKey "AcBkwCatLigColl"
    let catRacemCollKey = ConfigKey "CatRacemColl"
    let enCatRacemCollKey = ConfigKey "EnCatRacemColl"
    let acCatRacemCollKey = ConfigKey "AcCatRacemColl"
    let sedDirCollKey = ConfigKey "SedDirColl"
    let acCollKey = ConfigKey "AcColl"
    

    let earlyExitCheckFreqKey = ConfigKey "EarlyExitCheckFrequencyInMinutes"
    let quickProgressKey = ConfigKey "QuickProgress"
    let quickMinEeKey = ConfigKey "QuickMinEe"
    let standardProgressKey = ConfigKey "StandardProgress"
    let standardMinEeKey = ConfigKey "StandardMinEe"
    let slowProgressKey = ConfigKey "SlowProgress"
    let slowMinEeKey = ConfigKey "SlowMinEe"
    let maxRunTimeKey = ConfigKey "MaxRunTimeInDays"
    
    let resultLocationKey = ConfigKey "ResultLocation"
    let noOfProgressPointsKey = ConfigKey "NoOfProgressPoints"


    type AppSettingsProvider
        with

        member provider.trySetPairCollisionResolution key (c : PairCollisionResolution) =
            provider.trySet key (c.serialize())

        member provider.trySetTripleCollisionResolution key (c : TripleCollisionResolution) =
            provider.trySet key (c.serialize())

        /// Generated.
        member provider.trySetCollisionData (d : CollisionData) =
            [
                provider.trySetPairCollisionResolution sugSynthCollKey d.sugSynthColl
                provider.trySetPairCollisionResolution catSynthCollKey d.catSynthColl
                provider.trySetTripleCollisionResolution enCatSynthCollKey d.enCatSynthColl
                provider.trySetPairCollisionResolution acCatSynthCollKey d.acCatSynthColl
                provider.trySetPairCollisionResolution catDestrCollKey d.catDestrColl
                provider.trySetTripleCollisionResolution enCatDestrCollKey d.enCatDestrColl
                provider.trySetPairCollisionResolution acCatDestrCollKey d.acCatDestrColl
                provider.trySetPairCollisionResolution catLigCollKey d.catLigColl
                provider.trySetTripleCollisionResolution enCatLigCollKey d.enCatLigColl
                provider.trySetPairCollisionResolution acFwdCatLigCollKey d.acFwdCatLigColl
                provider.trySetPairCollisionResolution acBkwCatLigCollKey d.acBkwCatLigColl
                provider.trySetPairCollisionResolution catRacemCollKey d.catRacemColl
                provider.trySetTripleCollisionResolution enCatRacemCollKey d.enCatRacemColl
                provider.trySetPairCollisionResolution acCatRacemCollKey d.acCatRacemColl
                provider.trySetPairCollisionResolution sedDirCollKey d.sedDirColl
                provider.trySetPairCollisionResolution acCollKey d.acColl
            ]
            
            
        member provider.trySetEarlyExitParam(d : EarlyExitParam) =
            [
                provider.trySet earlyExitCheckFreqKey (int d.earlyExitCheckFreq.value.TotalMinutes)
                provider.trySet quickProgressKey d.quickProgress
                provider.trySet quickMinEeKey d.quickMinEe
                provider.trySet standardProgressKey d.standardProgress
                provider.trySet standardMinEeKey d.standardMinEe
                provider.trySet slowProgressKey d.slowProgress
                provider.trySet slowMinEeKey d.slowMinEe
                provider.trySet maxRunTimeKey d.maxRunTime.TotalDays
            ]


    type ContGenSettings
        with

        member w.trySaveSettings() =
            let toErr e = e |> ContGenSettingExn |> ContGenSettingsErr |> ContGenServiceErr |> Error

            match w.isValid(), AppSettingsProvider.tryCreate appSettingsFile with
            | Ok(), Ok provider ->
                try
                    updateContGenSettings provider w.contGenSvcInfo w.contGenCommType
                    updateMessagingSettings provider w.messagingSvcInfo w.messagingCommType

                    provider.trySet partitionerId w.contGenInfo.partitionerId.value.value |> ignore
                    provider.trySet resultLocationKey w.contGenInfo.resultLocation |> ignore
                    provider.trySet lastAllowedNodeErrInMinutes (w.contGenInfo.lastAllowedNodeErr.value / 1<minute>) |> ignore
                    provider.trySet dictionaryUpdateType w.contGenInfo.dictionaryUpdateType |> ignore
                    provider.trySetCollisionData w.contGenInfo.collisionData |> ignore

                    provider.trySet minUsefulEe w.contGenInfo.controlData.minUsefulEe.value |> ignore
                    provider.trySet absoluteTolerance w.contGenInfo.controlData.absoluteTolerance.value |> ignore
                    provider.trySet noOfProgressPointsKey w.contGenInfo.controlData.noOfProgressPoints |> ignore
                    
                    w.contGenInfo.controlData.earlyExitParamOpt
                    |> Option.defaultValue EarlyExitParam.defaultValue
                    |> provider.trySetEarlyExitParam
                    |> ignore

                    provider.trySave() |> Rop.bindError toErr
                with
                | e -> toErr e
            | Error e, _ -> Error e
            | _, Error e -> toErr e


    type AppSettingsProviderResult = Result<AppSettingsProvider, exn>


    let getServiceAddress (providerRes : AppSettingsProviderResult) n d =
        match providerRes with
        | Ok provider ->
            match provider.tryGetString n with
            | Ok (Some EmptyString) -> d
            | Ok (Some s) -> s
            | _ -> d
        | _ -> d
        |> ServiceAddress


    let getServiceHttpPort (providerRes : AppSettingsProviderResult) n d =
        match providerRes with
        | Ok provider ->
            match provider.tryGetInt n with
            | Ok (Some k) when k > 0 -> k
            | _ -> d
        | _ -> d
        |> ServicePort


    let getServiceNetTcpPort (providerRes : AppSettingsProviderResult) n d =
        match providerRes with
        | Ok provider ->
            match provider.tryGetInt n with
            | Ok (Some k) when k > 0 -> k
            | _ -> d
        | _ -> d
        |> ServicePort


    let getCommunicationType (providerRes : AppSettingsProviderResult) n d =
        match providerRes with
        | Ok provider ->
            match provider.tryGetString n with
            | Ok (Some s) -> WcfCommunicationType.tryCreate s |> Option.defaultValue NetTcpCommunication
            | _ -> d
        | _ -> d


    let getPartitionerId (providerRes : AppSettingsProviderResult) n d =
        match providerRes with
        | Ok provider ->
            match provider.tryGetGuid n with
            | Ok (Some p) when p <> Guid.Empty -> p |> MessagingClientId |> PartitionerId
            | _ -> d
        | _ -> d


    let tryCreateCollisionType s =
        match s with
        | nameof(NoCollisionResolution) -> NoCollisionResolution |> Some |> Ok
        | nameof(ExcludeDuplicates) -> ExcludeDuplicates |> Some |> Ok
        | EmptyString -> None |> Ok
        | _ -> Error s


    let getCollisionData (provider : AppSettingsProvider) =
        let getPairCollision defaultValue key = (PairCollisionResolution.tryDeserialize, key) ||> provider.tryGetOrDefault defaultValue
        let getTripleCollision defaultValue key = (TripleCollisionResolution.tryDeserialize, key) ||> provider.tryGetOrDefault defaultValue

        {
            sugSynthColl = getPairCollision PairCollisionResolution.defaultValue sugSynthCollKey
            catSynthColl = getPairCollision PairCollisionResolution.defaultValue catSynthCollKey
            enCatSynthColl = getTripleCollision TripleCollisionResolution.defaultValue enCatSynthCollKey
            acCatSynthColl = getPairCollision PairCollisionResolution.defaultValue acCatSynthCollKey
            catDestrColl = getPairCollision PairCollisionResolution.defaultValue catDestrCollKey
            enCatDestrColl = getTripleCollision TripleCollisionResolution.defaultValue enCatDestrCollKey
            acCatDestrColl = getPairCollision PairCollisionResolution.defaultValue acCatDestrCollKey
            catLigColl = getPairCollision PairCollisionResolution.defaultValue catLigCollKey
            enCatLigColl = getTripleCollision TripleCollisionResolution.defaultValue enCatLigCollKey
            acFwdCatLigColl = getPairCollision PairCollisionResolution.defaultValue acFwdCatLigCollKey
            acBkwCatLigColl = getPairCollision PairCollisionResolution.defaultValue acBkwCatLigCollKey
            catRacemColl = getPairCollision PairCollisionResolution.defaultValue catRacemCollKey
            enCatRacemColl = getTripleCollision TripleCollisionResolution.defaultValue enCatRacemCollKey
            acCatRacemColl = getPairCollision PairCollisionResolution.defaultValue acCatRacemCollKey
            sedDirColl = getPairCollision PairCollisionResolution.defaultValue sedDirCollKey
            acColl = getPairCollision PairCollisionResolution.defaultValue acCollKey
        }


    let loadMessagingSettings providerRes =
        let messagingServiceCommunicationType = getCommunicationType providerRes messagingServiceCommunicationType NetTcpCommunication
        let serviceAddress = getServiceAddress providerRes messagingServiceAddress defaultMessagingServiceAddress
        let httpServicePort = getServiceHttpPort providerRes messagingHttpServicePort defaultMessagingHttpServicePort
        let netTcpServicePort = getServiceNetTcpPort providerRes messagingNetTcpServicePort defaultMessagingNetTcpServicePort

        let h = HttpServiceAccessInfo.create serviceAddress httpServicePort messagingHttpServiceName.value
        let n = NetTcpServiceAccessInfo.create serviceAddress netTcpServicePort messagingNetTcpServiceName.value WcfSecurityMode.defaultValue
        let m = ServiceAccessInfo.create h n
        let messagingSvcInfo = MessagingServiceAccessInfo.create messagingDataVersion m

        messagingSvcInfo, messagingServiceCommunicationType


    let loadContGenServiceSettings providerRes =
        let contGenServiceAddress = getServiceAddress providerRes contGenServiceAddress defaultContGenServiceAddress
        let contGenServiceHttpPort = getServiceHttpPort providerRes contGenServiceHttpPort defaultContGenHttpServicePort
        let contGenServiceNetTcpPort = getServiceNetTcpPort providerRes contGenServiceNetTcpPort defaultContGenNetTcpServicePort
        let contGenServiceCommunicationType = getCommunicationType providerRes contGenServiceCommunicationType NetTcpCommunication

        let contGenSvcInfo = ContGenServiceAccessInfo.create contGenServiceAddress contGenServiceHttpPort contGenServiceNetTcpPort WcfSecurityMode.defaultValue

        (contGenSvcInfo, contGenServiceCommunicationType)
        
        
    /// Gets the value out of provider's result or default.        
    let toValueOrDefault m d v =
        v                    
        |> Rop.toOption
        |> Option.flatten
        |> Option.map m
        |> Option.defaultValue d         
        
        
    let loadEarlyExitParam (provider : AppSettingsProvider) =
        let getProgress key defaultValue = provider.tryGetDecimal key |> Rop.toOption |> Option.flatten |> Option.defaultValue defaultValue
        let getEe key defaultValue = provider.tryGetDouble key |> Rop.toOption |> Option.flatten |> Option.defaultValue defaultValue
        let d = EarlyExitParam.defaultValue
        
        {
            earlyExitCheckFreq =
                provider.tryGetInt earlyExitCheckFreqKey
                |> toValueOrDefault (fun e -> TimeSpan.FromMinutes(double e) |> EarlyExitCheckFrequency) EarlyExitCheckFrequency.defaultValue
                
            quickProgress = getProgress quickProgressKey d.quickProgress
            quickMinEe = getEe quickMinEeKey d.quickMinEe
            standardProgress = getProgress standardProgressKey d.standardProgress
            standardMinEe = getEe standardMinEeKey d.standardMinEe
            slowProgress = getProgress slowProgressKey d.slowProgress
            slowMinEe = getEe slowMinEeKey d.slowMinEe            
            maxRunTime =provider.tryGetDouble maxRunTimeKey|> toValueOrDefault TimeSpan.FromDays d.maxRunTime
        }


    let loadContGenSettings() =
        let providerRes = AppSettingsProvider.tryCreate appSettingsFile

        let contGenInfo =
            match providerRes with
            | Ok provider ->
                {
                    partitionerId = getPartitionerId providerRes partitionerId defaultPartitionerId
                    resultLocation = provider.tryGetString resultLocationKey |> toValueOrDefault id DefaultResultLocationFolder

                    lastAllowedNodeErr =
                        match provider.tryGetInt lastAllowedNodeErrInMinutes with
                        | Ok (Some p) when p > 0 -> p * 1<minute> |> LastAllowedNodeErr
                        | _ -> LastAllowedNodeErr.defaultValue
                        
                    collisionData = getCollisionData provider

                    dictionaryUpdateType =
                        match provider.tryGet DictionaryUpdateType.tryDeserialize dictionaryUpdateType with
                        | Ok (Some v) -> v
                        | _ -> AllRateData
                        
                    controlData =
                        {
                            minUsefulEe =
                                match provider.tryGetDecimal minUsefulEe with
                                | Ok (Some ee) -> ee |> double |> MinUsefulEe
                                | _ -> MinUsefulEe.defaultValue

                            noOfProgressPoints = provider.tryGetInt noOfProgressPointsKey |> toValueOrDefault id defaultNoOfProgressPoints                           
                            earlyExitParamOpt = loadEarlyExitParam provider |> Some                       

                            absoluteTolerance =
                                match provider.tryGetDouble absoluteTolerance with
                                | Ok (Some t) -> t |> AbsoluteTolerance
                                | Error e ->
                                    printfn $"loadContGenSettings: {absoluteTolerance}, error: {e}."
                                    AbsoluteTolerance.defaultValue
                                | _ -> AbsoluteTolerance.defaultValue
                        }
                }
            | _ ->
                {
                    partitionerId = defaultPartitionerId
                    resultLocation = DefaultResultLocationFolder
                    lastAllowedNodeErr = LastAllowedNodeErr.defaultValue
                    collisionData = CollisionData.defaultValue
                    dictionaryUpdateType = AllRateData
                    
                    controlData =
                        {
                            minUsefulEe = MinUsefulEe.defaultValue
                            noOfProgressPoints = defaultNoOfProgressPoints                          
                            earlyExitParamOpt = Some EarlyExitParam.defaultValue
                            absoluteTolerance = AbsoluteTolerance.defaultValue                            
                        }
                }

        let contGenSvcInfo, contGenServiceCommunicationType = loadContGenServiceSettings providerRes
        let messagingSvcInfo, messagingServiceCommunicationType = loadMessagingSettings providerRes

        let w =
            {
                contGenInfo = contGenInfo
                contGenSvcInfo = contGenSvcInfo
                contGenCommType = contGenServiceCommunicationType
                messagingSvcInfo = messagingSvcInfo
                messagingCommType = messagingServiceCommunicationType
            }

//        printfn "loadContGenSettings: Using settings:\n%A" w

        w


    let saveContGenSettings loadSettings tryGetSaveSettings =
        let (w : ContGenSettings) = loadSettings()

        let r =
            match tryGetSaveSettings() with
            | Some() -> w.trySaveSettings()
            | None -> Ok()

        match r with
        | Ok() -> printfn "Successfully saved settings."
        | Error e -> printfn $"Error occurred trying to save settings: %A{e}."
