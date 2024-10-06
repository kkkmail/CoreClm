namespace ServiceProxy

open Clm.ModelParams
open ClmSys.ContGenPrimitives
open ClmSys.ClmErrors
open ClmSys.SolverData
open ClmSys.SolverRunnerPrimitives
open ContGenServiceInfo.ServiceInfo
open ClmSys.GeneralPrimitives
open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives
open Primitives.SolverRunnerErrors

module SolverRunner =
    let x = 1

//    type SolverUpdateProxy =
//        {
//            updateProgress : ProgressUpdateInfo -> UnitResult
////            updateTime : ProgressData -> UnitResult
//            checkCancellation : RunQueueId -> CancellationType option
//            logCrit : SolverRunnerCriticalError -> UnitResult
//        }


//    type SolverNotificationProxy =
//        {
//            checkNotificationRequest : RunQueueId -> ResultNotificationType option
//            clearNotificationRequest : RunQueueId -> UnitResult
//        }


//    type SolverRunnerProxy =
//        {
//            solverUpdateProxy : SolverUpdateProxy
//            solverNotificationProxy : SolverNotificationProxy
////            saveResult : ResultDataWithId -> UnitResult
//            saveCharts : ChartGenerationResult -> UnitResult
//            logCrit : SolverRunnerCriticalError -> UnitResult
//        }
