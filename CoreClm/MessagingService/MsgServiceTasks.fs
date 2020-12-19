﻿namespace MessagingService

open Argu
open ClmSys.MessagingData
open ClmSys.ClmWorker
open MessagingService.SvcCommandLine
open MessagingServiceInfo.ServiceInfo

module ServiceTasks =

    type MessagingConfigParam
        with
        static member fromParseResults (p : ParseResults<MessagingServiceRunArgs>) : list<MessagingConfigParam> =
            [
            ]
            |> List.choose id


    let getParams p = MessagingConfigParam.fromParseResults p, getServiceSettings (p.GetAllResults())
    let getSaveSettings (p : ParseResults<MessagingServiceRunArgs>) () = p.GetAllResults() |> saveSettings
    type MessagingServiceTask = WorkerTask<(list<MessagingConfigParam> * MsgSettings), MessagingServiceRunArgs>
