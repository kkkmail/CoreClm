namespace ServiceProxy

open Softellect.Messaging.ServiceInfo
open Clm.ModelParams
open ContGenServiceInfo.ServiceInfo
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.WorkerNodeData
open MessagingServiceInfo.ServiceInfo
open ClmSys.WorkerNodePrimitives
open ClmSys.Logging
open DbData.DatabaseTypes
open ServiceProxy.MsgProcessorProxy
open Clm.CalculationData
open ClmSys.ModelData
open NoSql.FileSystemTypes

module ModelRunnerProxy =

    type RunModelProxy =
        {
            sendRunModelMessage : MessageInfo -> UnitResult
            loadModelData : ModelDataId -> ClmResult<ModelData>
            controlData : RunnerControlData
        }


    type TryRunFirstModelProxy =
        {
            tryLoadFirstRunQueue : unit -> ClmResult<RunQueue option>
            tryGetAvailableWorkerNode : unit -> ClmResult<WorkerNodeId option>
            runModel : RunQueue -> UnitResult
            upsertRunQueue : RunQueue -> UnitResult
        }


    type TryCancelRunQueueProxy =
        {
            tryLoadRunQueue : RunQueueId -> ClmResult<RunQueue option>
            sendCancelRunQueueMessage : MessageInfo -> UnitResult
            upsertRunQueue : RunQueue -> UnitResult
        }


    type TryRequestResultsProxy =
        {
            tryLoadRunQueue : RunQueueId -> ClmResult<RunQueue option>
            sendRequestResultsMessage : MessageInfo -> UnitResult
        }


    type TryResetProxy =
        {
            tryResetRunQueue : RunQueueId -> UnitResult
        }


    type TryRunModelResult =
        | WorkScheduled
        | NoWork
        | NoAvailableWorkerNodes


    type TryRunAllModelsProxy =
        {
            tryRunFirstModel : unit -> ClmResult<TryRunModelResult>
        }


    type UpdateProgressProxy =
        {
            tryLoadRunQueue : RunQueueId -> ClmResult<RunQueue option>
            upsertRunQueue : RunQueue -> UnitResult
            upsertWorkerNodeErr : WorkerNodeId -> UnitResult
        }

        static member create c =
            {
                tryLoadRunQueue = tryLoadRunQueue c
                upsertRunQueue = upsertRunQueue c
                upsertWorkerNodeErr = upsertWorkerNodeErr c
            }


    type RegisterProxy =
        {
            upsertWorkerNodeInfo : WorkerNodeInfo -> UnitResult
        }

        static member create c =
            {
                upsertWorkerNodeInfo = upsertWorkerNodeInfo c
            }


    type UnregisterProxy =
        {
            loadWorkerNodeInfo : WorkerNodeId -> ClmResult<WorkerNodeInfo>
            upsertWorkerNodeInfo : WorkerNodeInfo -> UnitResult
        }
        static member create c =
            {
                loadWorkerNodeInfo = loadWorkerNodeInfo c
                upsertWorkerNodeInfo = upsertWorkerNodeInfo c
            }


    type SaveChartsProxy =
        {
            saveCharts : ChartInfo -> UnitResult
        }

        static member create resultLocation =
            {
                saveCharts = fun (c : ChartInfo) -> saveLocalChartInfo (Some (resultLocation, c.defaultValueId)) c
            }


    type ProcessMessageProxy =
        {
            updateProgress : ProgressUpdateInfo -> UnitResult
            saveCharts : ChartInfo -> UnitResult
            register : WorkerNodeInfo -> UnitResult
            unregister : WorkerNodeId -> UnitResult
        }


    type GetRunStateProxy =
        {
            loadRunQueueProgress : unit -> ListResult<RunQueue>
        }

        static member create c =
            {
                loadRunQueueProgress = fun () -> loadRunQueueProgress c
            }


    type RunnerProxy =
        {
            getMessageProcessorProxy : MessagingClientAccessInfo -> MessageProcessorProxy
        }


    type RunnerData =
        {
            getConnectionString : unit -> ConnectionString
            resultLocation : string
            lastAllowedNodeErr : LastAllowedNodeErr
            collisionData : CollisionData
            dictionaryUpdateType : DictionaryUpdateType
            controlData : RunnerControlData
        }


    type RunModelProxy
        with
        static member create (d : RunnerData) s =
            {
                sendRunModelMessage = s
                loadModelData = loadModelData d.getConnectionString
                controlData = d.controlData
            }


    type RunnerDataWithProxy =
        {
            runnerData : RunnerData
            messageProcessorProxy : MessageProcessorProxy
        }


    type ModelRunnerDataWithProxy =
        {
            runnerData : RunnerData
            runnerProxy : RunnerProxy
            messagingClientAccessInfo : MessagingClientAccessInfo
            logger : Logger
        }
