namespace Primitives

open GeneralPrimitives
open SolverPrimitives
open GeneralErrors
open Softellect.Messaging.Primitives
open Softellect.Messaging.Errors

module SolverRunnerErrors =

    type SolverRunnerCriticalError =
        {
            errorId : ErrorId
            runQueueId : RunQueueId
            errorMessage : string
        }

        static member create q e =
            {
                errorId = ErrorId.getNewId()
                runQueueId = q
                errorMessage = $"{e}"
            }

    type OnSaveChartsError =
        | SendChartMessageErr of (MessagingClientId * RunQueueId * MessagingError)


    type OnUpdateProgressError =
        | UnableToSendProgressMsgErr of RunQueueId
        | UnableToFindMappingErr of RunQueueId


    //type CheckRunningResult =
    //    | CanRun
    //    | AlreadyRunning of ProcessId
    //    | TooManyRunning of int
    //    | GetProcessesByNameExn of exn


    type SolverRunnerError =
        | OnSaveChartsErr of OnSaveChartsError
        | OnUpdateProgressErr of OnUpdateProgressError


    ///// See: https://stackoverflow.com/questions/49974736/how-to-declare-a-generic-exception-types-in-f
    ///// We have to resort to throwing a specific exception in order
    ///// to perform early termination from deep inside C# ODE solver.
    ///// There seems to be no other easy and clean way. Revisit if that changes.
    //// type ComputationAbortedException<'T> (pd : ProgressData<'T>, ct : CancellationType) =
    //type ComputationAbortedException (pd : ProgressData, ct : CancellationType) =
    //    inherit System.Exception ()

    //    member e.progressData = pd
    //    member e.cancellationType = ct
