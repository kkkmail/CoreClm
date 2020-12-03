namespace ClmSys

open Softellect.Sys.TimerEvents
open ClmErrors

module TimerEvents =

    let private toErr e = e |> TimerEventErr


    type ClmEventHandlerInfo =
        {
            timerEventInfo : TimerEventInfo
            timerProxy : TimerEventProxy<ClmError>
        }

        member i.withFirstDelay d =
            { i with timerEventInfo = { i.timerEventInfo with firstDelay = d } }

        static member defaultValue logger h n =
            {
                timerEventInfo = TimerEventInfo.defaultValue n
                timerProxy =
                    {
                        eventHandler = h
                        logger = logger
                        toErr = toErr
                    }
            }

        static member oneHourValue logger h n =
            {
                timerEventInfo = TimerEventInfo.oneHourValue n
                timerProxy =
                    {
                        eventHandler = h
                        logger = logger
                        toErr = toErr
                    }
            }


    type ClmEventHandler(i : ClmEventHandlerInfo) =
        inherit TimerEventHandler<ClmError>(i.timerEventInfo, i.timerProxy)
