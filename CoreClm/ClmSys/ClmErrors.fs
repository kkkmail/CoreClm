namespace ClmSys

open Softellect.Sys.WcfErrors
open Softellect.Sys.TimerErrors
open Softellect.Sys.Rop
open Softellect.Sys.MessagingClientErrors
open Softellect.Sys.MessagingServiceErrors

open GeneralErrors
open WorkerNodeErrors
open ModelGeneratorErrors
open ModelRunnerErrors
open ContGenErrors
open SolverRunnerPrimitives
open SolverRunnerErrors

module ClmErrors =

    /// All errors known in the system.
    type ClmError =
        | AggregateErr of ClmError * List<ClmError>
        | WcfErr of WcfError
        | TimerEventErr of TimerEventError
        | MessagingServiceErr of MessagingServiceError
        | MessagingClientErr of MessagingClientError
        | ClmEventHandlerErr of ClmEventHandlerError

        | UnhandledExn of string * exn
        | ServiceInstallerErr of ServiceInstallerError
        | RegistryErr of RegistryError
        | FileErr of FileError
        | SerializationErr of SerializationError
        | DbErr of DbError
        | ModelGeneratorErr of ModelGeneratorError
        | WorkerNodeErr of WorkerNodeError
        | SolverRunnerErr of SolverRunnerError
        | WorkerNodeServiceErr of WorkerNodeServiceError
        | ModelRunnerErr of ModelRunnerError
        | ContGenServiceErr of ContGenServiceError

        static member addError a b =
            match a, b with
            | AggregateErr (x, w), AggregateErr (y, z) -> AggregateErr (x, w @ (y :: z))
            | AggregateErr (x, w), _ -> AggregateErr (x, w @ [b])
            | _, AggregateErr (y, z) -> AggregateErr (a, y :: z)
            | _ -> AggregateErr (a, [b])

        static member (+) (a, b) = ClmError.addError a b
        member a.add b = a + b


    let (>->) s1 s2 =
        match s1() with
        | Ok() -> s2
        | Error e -> fun () -> Error e


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


    /// We have to resort to throwing a specific exception in order
    /// to perform early termination from deep inside C# ODE solver.
    exception ComputationAbortedException of (ProgressData * CancellationType)
