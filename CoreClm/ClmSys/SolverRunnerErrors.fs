namespace ClmSys

open GeneralErrors
open ExitErrorCodes

module SolverRunnerErrors =

    type CriticalErrorType =
        | ErrorCodeBased
        | ExceptionBased
        | ErrroMessageBased


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
                errorType = ErrroMessageBased
                result = UnknownException
                errorMessageOpt = Some e
                exceptionOpt = None
            }
