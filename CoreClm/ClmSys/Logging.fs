namespace ClmSys

open System
open Softellect.Sys.Logging
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


    type LogMessage<'E>
        with
        member d.level =
            match d.logLevel with
            | CritLog -> Level.Critical
            | ErrLog -> Level.Error
            | WarnLog -> Level.Warn
            | InfoLog -> Level.Info
            | DebugLog -> Level.Debug

        member d.message = d.logData.ToString()


    type LogMessage = LogMessage<ClmError>


    let logAgent = MailboxProcessor.Start <| fun inbox ->
        let rec logLoop () = async {
            let! (message : LogMessage) = inbox.Receive()
            printfn "logAgent - logging message: %A" message
            let logData = LoggingEventData(Domain = AppDomain.CurrentDomain.FriendlyName, Level = message.level, Message = message.message, TimeStampUtc = DateTime.UtcNow, LoggerName = logName)
            let logEvent = LoggingEvent(logData)
            log4netLogger.Logger.Log logEvent
            return! logLoop()
        }
        logLoop ()


    type Logger = Logger<ClmError>


//        {
//            logError : ClmError -> unit
//            logWarn : ClmError -> unit
//            logInfo : ClmInfo -> unit
//        }
//
//        member this.logInfoString (s : string) = ClmInfo.create s |> this.logInfo
//        member this.logExn s e = this.logError (UnhandledExn (s, e))
//
//        member this.logIfError v =
//            match v with
//            | Ok _ -> ignore()
//            | Error e -> this.logError e
//
//
//        static member defaultValue =
//            {
//                logError = printfn "ERROR: %A"
//                logWarn = printfn "WARN: %A"
//                logInfo = printfn "INFO: %A"
//            }
//
//        /// The real log4net logger.
//        /// If you are on the edge, e.g. SolverRunner, and printfn is absolutely unavailalbe then use this.
//        static member log4netImpl =
//            {
//                logError =
//                    fun e ->
//                        {
//                            Message = sprintf "ERROR: %A" e
//                            Date = DateTime.Now
//                        }
//                        |> InfoMessage
//                        |> logAgent.Post
//
//                logWarn =
//                    fun e ->
//                        {
//                            Message = sprintf "WARN: %A" e
//                            Date = DateTime.Now
//                        }
//                        |> InfoMessage
//                        |> logAgent.Post
//
//                logInfo =
//                    fun e ->
//                        {
//                            Message = sprintf "INFO: %A" e
//                            Date = DateTime.Now
//                        }
//                        |> InfoMessage
//                        |> logAgent.Post
//            }
//
//        /// The twisty log4net logger. It could be real log4net or it could be just printfn.
//        /// Use it when printfn is available and you would like to get all possible output.
//        /// Twist it to either real log4net or printfn depending on the needs.
//        static member log4net = Logger.log4netImpl


    let logger = Logger.defaultValue
