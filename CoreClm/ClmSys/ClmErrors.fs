namespace ClmSys

open GeneralErrors
open MessagingServiceErrors
open MessagingClientErrors
open WorkerNodeErrors
open ModelGeneratorErrors
open ModelRunnerErrors
open GeneralPrimitives
open ContGenErrors
open SolverRunnerPrimitives

module ClmErrors =

    /// All errors known in the system.
    type ClmError =
        | AggregateErr of ClmError * List<ClmError>
        | WcfErr of WcfError
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
        | WorkerNodeServiceErr of WorkerNodeServiceError
        | ModelRunnerErr of ModelRunnerError
        | ContGenServiceErr of ContGenServiceError

        static member (+) (a, b) =
            match a, b with
            | AggregateErr (x, w), AggregateErr (y, z) -> AggregateErr (x, w @ (y :: z))
            | AggregateErr (x, w), _ -> AggregateErr (x, w @ [b])
            | _, AggregateErr (y, z) -> AggregateErr (a, y :: z)
            | _ -> AggregateErr (a, [b])

        member a.add b = a + b


    let (>->) s1 s2 =
        match s1() with
        | Ok() -> s2
        | Error e -> fun () -> Error e


    let evaluate f = f()


    /// Encapsulation of logging information
    type ClmInfo =
        | ClmInfo of string

        static member create a = sprintf "%A" a |> ClmInfo


    /// Type to encapsulate the result of an action, which can only have success or failure.
    /// For example, "save object", "delete object" fall into this category.
    type UnitResult = Result<unit, ClmError>


    /// kk:20200129 - I am not sure if this is extremely useful. Let's see how it goes.
    /// Type to encapsulate the result of an action, which may have success / no result / failure.
    /// For example "try delete object" may return that object was deleted OR it does not exist OR it produced an exception.
    /// This is a "reverse" for "try load", which should return Result<'T option, 'E>.
    type TryResult = Result<unit option, ClmError>


    type ClmResult<'T> = Result<'T, ClmError>
    type ListResult<'T> = Result<list<Result<'T, ClmError>>, ClmError>
    type StateWithResult<'T> = 'T * UnitResult


    /// ! Note that we cannot elevate to Result here as it will broaden the scope !
    /// Folds list<ClmError> in a single ClmError.
    let foldErrors (a : list<ClmError>) =
        match a with
        | [] -> None
        | h :: t -> t |> List.fold (fun acc r -> r + acc) h |> Some


    /// Converts an error option into a unit result.
    let toUnitResult fo =
        match fo with
        | None -> Ok()
        | Some f -> Error f


    /// Folds list<ClmError>, then converts to UnitResult.
    let foldToUnitResult = foldErrors >> toUnitResult


    /// Adds error f if the result is (Error e).
    /// Otherwise returns then same (Ok r).
    let addError v (f : ClmError) =
        match v with
        | Ok r -> Ok r
        | Error e -> Error (f + e)


    /// The first result r1 is an earlier result and r2 is a later result
    /// and we want to sum up errors as (e2 + e1), so that to keep
    /// the latest error at the beginning.
    let combineUnitResults (r1 : UnitResult) (r2 : UnitResult) =
        match r1, r2 with
        | Ok(), Ok() -> Ok()
        | Error e1, Ok() -> Error e1
        | Ok(), Error e2 -> Error e2
        | Error e1, Error e2 -> Error (e2 + e1)


    let toErrorOption f g (r : UnitResult) =
        match r with
        | Ok() -> None
        | Error e -> Some ((f g) + e)


    /// The head should contain the latest error and the tail the earliest error.
    let foldUnitResults (r : list<UnitResult>) =
        let rec fold acc w =
            match w with
            | [] -> acc
            | h :: t -> fold (combineUnitResults h acc) t

        fold (Ok()) r


    type ClmErrorInfo =
        {
            errorId : ErrorId
            traceInfo : TraceInfo
            error : ClmError
        }


    /// We have to resort to throwing a specific exception in order
    /// to perform early termination from deep inside C# ODE solver.
    exception ComputationAbortedException of (RunQueueId * CancellationType)
