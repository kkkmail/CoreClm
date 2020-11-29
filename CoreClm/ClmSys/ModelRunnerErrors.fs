namespace ClmSys

open GeneralPrimitives
open ContGenPrimitives
open WorkerNodePrimitives
open MessagingPrimitives
open MessagingClientErrors

module ModelRunnerErrors =

    type RunModelError =
        | MissingWorkerNodeErr of RunQueueId
        | UnableToLoadModelDataErr of RunQueueId * ModelDataId


    type TryRunFirstModelError =
        | TryLoadFirstRunQueueErr
        | TryGetAvailableWorkerNodeErr
        | UpsertRunQueueErr
        | UnableToRunModelErr
        | UnableToRunModelAndUpsertStatusErr


    type TryCancelRunQueueError =
        | TryLoadRunQueueErr of RunQueueId
        | InvalidRunQueueStatusErr of RunQueueId


    type TryRequestResultsError =
        | TryLoadRunQueueErr of RunQueueId

    type TryRunAllModelsError =
        | UnableToTryRunFirstModelErr


    type UpdateProgressError =
        | UnableToLoadRunQueueErr of RunQueueId
        | UnableToFindLoadRunQueueErr of RunQueueId
        | InvalidRunQueueStatusErr of RunQueueId
        | CompleteyInvalidRunQueueStatusErr of RunQueueId // This should never happen but we still have to account for it. It if does, then we are in a BIG trouble.


    type RegisterError =
        | UnableToUpsertWorkerNodeInfoErr of WorkerNodeId


    type UnregisterError =
        | UnableToLoadWorkerNodeInfoErr of WorkerNodeId
        | UnableToUpsertWorkerNodeInfoOnUnregisterErr of WorkerNodeId


    type SaveResultError =
        | UnableToSaveResultDataErr of ResultDataId


    type SaveChartsError =
        | UnableToSaveCharts of ResultDataId


    type ProcessMessageError =
        | ErrorWhenProcessingMessageErr of MessageId
        | InvalidMessageTypeErr of MessageId
        | OnGetMessagesErr of OnGetMessagesError


    type ModelRunnerError =
        | RunModelErr of RunModelError
        | TryRunFirstModelErr of TryRunFirstModelError
        | TryCancelRunQueueErr of TryCancelRunQueueError
        | TryRequestResultsErr of TryRequestResultsError
        | TryRunAllModelsErr of TryRunAllModelsError
        | UpdateProgressErr of UpdateProgressError
        | RegisterErr of RegisterError
        | UnregisterErr of UnregisterError
        | SaveResultErr of SaveResultError
        | SaveChartsErr of SaveChartsError
        | ProcessMessageErr of ProcessMessageError
