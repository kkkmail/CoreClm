namespace ClmSys

open ClmSys.GeneralPrimitives
open GeneralErrors
open ExitErrorCodes
open Softellect.Sys.MessagingPrimitives

module SolverRunnerErrors =

    type CriticalErrorType =
        | ErrorCodeBased
        | ExceptionBased
        | ErrorMessageBased


    type SolverRunnerCriticalError =
        {
            errorId : ErrorId
            errorType : CriticalErrorType
            result : int
            errorMessageOpt : string option
            exceptionOpt : exn option
        }

        static member fromErrorCode e =
            {
                errorId = ErrorId.getNewId()
                errorType = ErrorCodeBased
                result = e
                errorMessageOpt = None
                exceptionOpt = None
            }

        static member fromExn e =
            {
                errorId = ErrorId.getNewId()
                errorType = ExceptionBased
                result = UnknownException
                errorMessageOpt = None
                exceptionOpt = Some e
            }


        static member fromErrMessage e =
            {
                errorId = ErrorId.getNewId()
                errorType = ErrorMessageBased
                result = UnknownException
                errorMessageOpt = Some e
                exceptionOpt = None
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
