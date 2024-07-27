namespace ClmSys

open Primitives.GeneralErrors
open Primitives.SolverPrimitives
open Softellect.Wcf.Errors
open Softellect.Sys.Errors
open Softellect.Sys.Rop
open Softellect.Messaging.Errors

open GeneralErrors
open WorkerNodeErrors
open ModelGeneratorErrors
open ModelRunnerErrors
open ContGenErrors
open Primitives.SolverRunnerErrors

module ClmErrors =

    /// All errors known in the system.
    type ClmError =
        | AggregateErr of ClmError * List<ClmError>

        // dbo schema
        | TryResetRunQueueErr of TryResetRunQueueError
        | SaveRunQueueErr of SaveRunQueueError
        | DeleteRunQueueErr of DeleteRunQueueError
        | TryUpdateRunQueueRowErr of TryUpdateRunQueueRowError
        | UpsertRunQueueErr of UpsertRunQueueError
        | TimerEventErr of TimerEventError
        | LoadWorkerNodeInfoErr of LoadWorkerNodeInfoError
        | UpsertWorkerNodeInfoErr of UpsertWorkerNodeInfoError
        | UpsertWorkerNodeErrErr of UpsertWorkerNodeErrError
        | TryGetAvailableWorkerNodeErr of TryGetAvailableWorkerNodeError

        // clm schema
        | LoadClmDefaultValueErr of LoadClmDefaultValueError
        | UpsertClmDefaultValueErr of UpsertClmDefaultValueError
        | LoadCommandLineParamsErr of LoadCommandLineParamsError
        | AddCommandLineParamsErr of AddCommandLineParamsError
        | TryCreateClmTaskErr of TryCreateClmTaskError
        | LoadClmTaskErr of LoadClmTaskError
        | LoadIncompleteClmTasksErr of LoadIncompleteClmTasksError
        | AddClmTaskErr of AddClmTaskError
        | UpdateClmTaskErr of UpdateClmTaskError
        | TryCreateModelDataErr of TryCreateModelDataError
        | LoadModelDataErr of LoadModelDataError
        | UpsertModelDataErr of UpsertModelDataError
        //| MapRunQueueErr of MapRunQueueError
        //| LoadRunQueueErr of LoadRunQueueError
        //| LoadRunQueueProgressErr of LoadRunQueueProgressError
        //| TryLoadFirstRunQueueErr of TryLoadFirstRunQueueError
        //| TryLoadRunQueueErr of TryLoadRunQueueError

        // eeinf schema

        // ========================
        // WorkerNode
        //| TryLoadSolverRunnersErr of TryLoadSolverRunnersError
        //| TryGetRunningSolversCountErr of TryGetRunningSolversCountError
        //| TryPickRunQueueErr of TryPickRunQueueError
        //| LoadAllActiveRunQueueIdErr of LoadAllActiveRunQueueIdError
        //| TryStartRunQueueErr of TryStartRunQueueError
        //| TryCompleteRunQueueErr of TryCompleteRunQueueError
        //| TryCancelRunQueueErr of TryCancelRunQueueError
        //| TryFailRunQueueErr of TryFailRunQueueError
        //| TryRequestCancelRunQueueErr of TryRequestCancelRunQueueError
        //| TryNotifyRunQueueErr of TryNotifyRunQueueError
        //| TryCheckCancellationErr of TryCheckCancellationError
        //| TryCheckNotificationErr of TryCheckNotificationError
        //| TryClearNotificationErr of TryClearNotificationError
        //| TryUpdateProgressErr of TryUpdateProgressError

        //=========================

        | InvalidSettings of string
        | SettingExn of exn
        | CreateModelRunnerMessageProcessorErr of MessagingError

        //=========================

        //| OnSaveChartsErr of OnSaveChartsError
        //| OnUpdateProgressErr of OnUpdateProgressError

        //=========================

        // These likely need to moved into better places.
        | WorkerNodeErr of WorkerNodeError
        | ContGenServiceErr of ContGenServiceError
        | FileErr of FileError
        | ModelGeneratorErr of ModelGeneratorError
        | SolverRunnerErr of SolverRunnerError
        | WorkerNodeServiceErr of WorkerNodeServiceError
        | ModelRunnerErr of ModelRunnerError

        //=========================
        | SendMessageErr of SendMessageError

        //| WcfErr of WcfError
        //| MessagingeErr of MessagingError
        //| ClmEventHandlerErr of ClmEventHandlerError

        //| UnhandledExn of string * exn
        //| ServiceInstallerErr of ServiceInstallerError
        ////| RegistryErr of RegistryError
        //| SerializationErr of SerializationError
        //| ClmDbErr of ClmDbError

        static member addError a b =
            match a, b with
            | AggregateErr (x, w), AggregateErr (y, z) -> AggregateErr (x, w @ (y :: z))
            | AggregateErr (x, w), _ -> AggregateErr (x, w @ [b])
            | _, AggregateErr (y, z) -> AggregateErr (a, y :: z)
            | _ -> AggregateErr (a, [b])

        static member (+) (a, b) = ClmError.addError a b
        member a.add b = a + b


    let evaluate f = f()


    type UnitResult = UnitResult<ClmError>
    type ClmResult<'T> = Result<'T, ClmError>
    type ListResult<'T> = ListResult<'T, ClmError>
    type StateWithResult<'T> = StateWithResult<'T, ClmError>

    let foldErrors a = foldErrors ClmError.addError a
    let foldToUnitResult = foldErrors >> toUnitResult
    let addError v f = addError ClmError.addError v f
    let toErrorOption f g r = toErrorOption ClmError.addError f g r


    /// The first result r1 is an earlier result and r2 is a later result
    /// and we want to sum up errors as (e2 + e1), so that to keep
    /// the latest error at the beginning.
    let combineUnitResults (r1 : UnitResult) (r2 : UnitResult) = combineUnitResults ClmError.addError r1 r2


    /// The head should contain the latest error and the tail the earliest error.
    let foldUnitResults r = foldUnitResults ClmError.addError r


    type ClmErrorInfo =
        {
            errorId : ErrorId
            traceInfo : TraceInfo
            error : ClmError
        }


    // /// We have to resort to throwing a specific exception in order
    // /// to perform early termination from deep inside C# ODE solver.
    // /// There seems to be no other easy and clean way. Revisit if that changes.
    // exception ComputationAbortedException of (ProgressData * CancellationType)
