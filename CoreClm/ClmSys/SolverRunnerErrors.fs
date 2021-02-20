namespace ClmSys

open ClmSys.GeneralPrimitives
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
        | SendResultMessageError of (MessagingClientId * ResultDataId)


    type OnSaveChartsError =
        | SendChartMessageError of (MessagingClientId * ResultDataId)


    type OnUpdateProgressError =
        | UnableToSendProgressMsgErr of RunQueueId
        | UnableToFindMappingErr of RunQueueId


    type SolverRunnerError =
        | OnSaveResultErr of OnSaveResultError
        | OnSaveChartsErr of OnSaveChartsError
        | OnUpdateProgressErr of OnUpdateProgressError
