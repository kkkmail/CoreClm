namespace ClmSys

open ClmSys.GeneralPrimitives
open ClmSys.SolverRunnerPrimitives
open GeneralErrors
open Softellect.Sys.MessagingPrimitives

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
        | SendChartMessageErr of (MessagingClientId * RunQueueId)


    type OnUpdateProgressError =
        | UnableToSendProgressMsgErr of RunQueueId
        | UnableToFindMappingErr of RunQueueId


    type CheckRunningResult =
        | CanRun
        | AlreadyRunning of ProcessId
        | TooManyRunning of int
        | GetProcessesByNameExn of exn


    type SolverRunnerError =
        | OnSaveChartsErr of OnSaveChartsError
        | OnUpdateProgressErr of OnUpdateProgressError
