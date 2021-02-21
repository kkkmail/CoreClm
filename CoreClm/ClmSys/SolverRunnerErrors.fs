namespace ClmSys

open ClmSys.GeneralPrimitives
open ClmSys.SolverRunnerPrimitives
open GeneralErrors
open Softellect.Sys.MessagingPrimitives

module SolverRunnerErrors =

    type SolverRunnerCriticalError =
        {
            errorId : ErrorId
            errorMessage : string
        }

        static member create e =
            {
                errorId = ErrorId.getNewId()
                errorMessage = $"{e}"
            }

    type OnSaveResultError =
        | SendResultMessageErr of (MessagingClientId * ResultDataId)


    type OnSaveChartsError =
        | SendChartMessageErr of (MessagingClientId * ResultDataId)


    type OnUpdateProgressError =
        | UnableToSendProgressMsgErr of RunQueueId
        | UnableToFindMappingErr of RunQueueId


    type CheckRunningResult =
        | CanRun
        | AlreadyRunning of ProcessId
        | TooManyRunning of int
        | GetProcessesByNameExn of exn

//    type CheckRunningError =
//        | AlreadyRunningErr of ProcessId
//        | GetProcessesByNameExn of exn


    type SolverRunnerError =
        | OnSaveResultErr of OnSaveResultError
        | OnSaveChartsErr of OnSaveChartsError
        | OnUpdateProgressErr of OnUpdateProgressError
//        | CheckRunningErr of CheckRunningError
