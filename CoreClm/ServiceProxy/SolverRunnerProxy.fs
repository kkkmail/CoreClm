namespace ServiceProxy

open Clm.ModelParams
open ClmSys.ContGenPrimitives
open ClmSys.ClmErrors
open ClmSys.SolverData
open ClmSys.SolverRunnerPrimitives
open ContGenServiceInfo.ServiceInfo
open ClmSys.SolverRunnerErrors
open ClmSys.GeneralPrimitives

module SolverRunner =

    type SolverUpdateProxy =
        {
            updateProgress : ProgressUpdateInfo -> UnitResult
            updateTime : ProgressData -> UnitResult
            checkCancellation : RunQueueId -> CancellationType option
        }


    type SolverNotificationProxy =
        {
            checkNotificationRequest : RunQueueId -> ResultNotificationType option
            clearNotificationRequest : RunQueueId -> UnitResult
        }


    type SolverRunnerProxy =
        {
            solverUpdateProxy : SolverUpdateProxy
            solverNotificationProxy : SolverNotificationProxy
            saveResult : ResultDataWithId -> UnitResult
            saveCharts : ChartGenerationResult -> UnitResult
            logCrit : SolverRunnerCriticalError -> UnitResult
        }
