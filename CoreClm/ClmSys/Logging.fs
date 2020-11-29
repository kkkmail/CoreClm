namespace ClmSys

open System
open System.Diagnostics
open log4net
open log4net.Core
open ClmErrors

module Logging =

    let logName = "ClmLog"

    //do
    //    printfn "Configuring log4net..."
    //    log4net.Config.XmlConfigurator.Configure()
    //    |> ignore
    //    printfn ".. done."


    let private log4netLogger = LogManager.GetLogger(logName)


    /// https://codereview.stackexchange.com/questions/202745/f-idiomatic-log4net-wrapper
    /// https://stackoverflow.com/questions/9805281/writing-logs-to-file
    /// https://codeshare.co.uk/blog/how-to-set-up-and-configure-error-logging-in-net-with-log4net/
    type LogInfo =
        {
            Message : string
            Date : DateTime
        }


    type ErrorInfo =
        {
            Error : exn
            StackTrace : StackTrace
        }


    type LogMessage =
        | DebugMessage of LogInfo
        | InfoMessage of LogInfo
        | WarningMessage of LogInfo
        | ErrMessage of ErrorInfo * LogInfo
        | FatalMessage of ErrorInfo * LogInfo

        member this.Message =
            match this with | DebugMessage i | InfoMessage i | WarningMessage i | ErrMessage (_, i) | FatalMessage (_, i) -> i.Message
        member this.Exception =
            match this with | ErrMessage (e, _) | FatalMessage (e, _) -> Some e.Error | _ -> None
        member this.Level =
            match this with | DebugMessage _ -> Level.Debug | InfoMessage _ -> Level.Info | WarningMessage _ -> Level.Warn | ErrMessage _ -> Level.Error | FatalMessage _ -> Level.Fatal
        member this.LogInfo =
            match this with | DebugMessage i | InfoMessage i | WarningMessage i | ErrMessage (_, i) | FatalMessage (_, i) -> i


    let logAgent = MailboxProcessor.Start <| fun inbox ->
        let rec logLoop () = async {
            let! (message : LogMessage) = inbox.Receive()
            printfn "logAgent - logging message: %A" message
            //writeLog message.Level message.LogInfo.Message message.Exception message.LogInfo.Date message.LogInfo.StackTrace
            let logData = LoggingEventData(Domain = AppDomain.CurrentDomain.FriendlyName, Level = message.Level, Message = message.LogInfo.Message, TimeStampUtc = message.LogInfo.Date, LoggerName = logName)
            let logEvent = LoggingEvent(logData)
            log4netLogger.Logger.Log logEvent
            return! logLoop()
        }
        logLoop ()


    type Logger =
        {
            logError : ClmError -> unit
            logWarn : ClmError -> unit
            logInfo : ClmInfo -> unit
        }

        member this.logInfoString (s : string) = ClmInfo.create s |> this.logInfo
        member this.logExn s e = this.logError (UnhandledExn (s, e))

        member this.logIfError v =
            match v with
            | Ok _ -> ignore()
            | Error e -> this.logError e


        static member defaultValue =
            {
                logError = printfn "ERROR: %A"
                logWarn = printfn "WARN: %A"
                logInfo = printfn "INFO: %A"
            }

        /// The real log4net logger.
        /// If you are on the edge, e.g. SolverRunner, and printfn is absolutely unavailalbe then use this.
        static member log4netImpl =
            {
                logError =
                    fun e ->
                        {
                            Message = sprintf "ERROR: %A" e
                            Date = DateTime.Now
                        }
                        |> InfoMessage
                        |> logAgent.Post

                logWarn =
                    fun e ->
                        {
                            Message = sprintf "WARN: %A" e
                            Date = DateTime.Now
                        }
                        |> InfoMessage
                        |> logAgent.Post

                logInfo =
                    fun e ->
                        {
                            Message = sprintf "INFO: %A" e
                            Date = DateTime.Now
                        }
                        |> InfoMessage
                        |> logAgent.Post
            }

        /// The twisty log4net logger. It could be real log4net or it could be just printfn.
        /// Use it when printfn is available and you would like to get all possible output.
        /// Twist it to either real log4net or printfn depending on the needs.
        static member log4net = Logger.log4netImpl


    let logger = Logger.defaultValue
