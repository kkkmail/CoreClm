namespace ClmSys

open Primitives.GeneralPrimitives
//open ClmSys.GeneralErrors
open Softellect.Messaging.Primitives
open Softellect.Messaging.Errors

open GeneralPrimitives
open ContGenPrimitives
open WorkerNodePrimitives

module ModelRunnerErrors =

    type RunModelRunnerError =
        | MessagingRunnerErr of MessagingError
        | MissingWorkerNodeRunnerErr of RunQueueId
        | UnableToLoadModelDataRunnerErr of RunQueueId * ModelDataId


    type TryRunFirstModelRunnerError =
        | TryLoadFirstRunQueueRunnerErr
        | TryGetAvailableWorkerNodelRunnerErr
        | UpsertRunQueueRunnerErr
        | UnableToRunModelRunnerErr
        | UnableToRunModelAndUpsertStatusRunnerErr
        | UnableToGetWorkerNodeRunnerErr


    type TryCancelRunQueueRunnerError =
        | TryLoadRunQueueRunnerErr of RunQueueId
        | InvalidRunQueueStatusRunnerErr of RunQueueId
        | MessagingTryCancelRunQueueRunnerErr of MessagingError


    type TryRequestResultsRunnerError =
        | TryLoadRunQueueRunnerErr of RunQueueId
        | MessagingTryRequestResultsRunnerErr of MessagingError

    type TryRunAllModelsRunnerError =
        | UnableToTryRunFirstModelRunnerErr


    type UpdateProgressRunnerError =
        | UnableToLoadRunQueueRunnerErr of RunQueueId
        | UnableToFindLoadRunQueueRunnerErr of RunQueueId
        | InvalidRunQueueStatusRunnerErr of RunQueueId
        | CompletelyInvalidRunQueueStatusRunnerErr of RunQueueId // This should never happen but we still have to account for it. It if does, then we are in a BIG trouble.


    type RegisterRunnerError =
        | UnableToUpsertWorkerNodeInfoRunnerErr of WorkerNodeId


    type UnregisterRunnerError =
        | UnableToLoadWorkerNodeInfoRunnerErr of WorkerNodeId
        | UnableToUpsertWorkerNodeInfoOnUnregisterRunnerErr of WorkerNodeId


    type SaveResultRunnerError =
        | UnableToSaveResultDataRunnerErr of RunQueueId


    type SaveChartsRunnerError =
        | UnableToSaveChartsRunnerErr of RunQueueId


    type ProcessMessageRunnerError =
        | ErrorWhenProcessingMessageRunnerErr of MessageId
        | InvalidMessageTypeRunnerErr of MessageId
        | OnGetMessagesRunnerErr of OnGetMessagesError


    type TryGetAvailableWorkerNodeRunnerError =
        | A


    type ModelRunnerError =
        | RunModelRunnerErr of RunModelRunnerError
        | TryRunFirstModelRunnerErr of TryRunFirstModelRunnerError
        | TryCancelRunQueueRunnerErr of TryCancelRunQueueRunnerError
        | TryRequestResultsRunnerErr of TryRequestResultsRunnerError
        | TryRunAllModelsRunnerErr of TryRunAllModelsRunnerError
        | UpdateProgressRunnerErr of UpdateProgressRunnerError
        | RegisterRunnerErr of RegisterRunnerError
        | UnregisterRunnerErr of UnregisterRunnerError
        | SaveResultRunnerErr of SaveResultRunnerError
        | SaveChartsRunnerErr of SaveChartsRunnerError
        | ProcessMessageRunnerErr of ProcessMessageRunnerError
        | TryGetAvailableWorkerNodeRunnerErr of TryGetAvailableWorkerNodeRunnerError
