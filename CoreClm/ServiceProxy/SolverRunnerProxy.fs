namespace ServiceProxy

open Clm.ModelParams
open ClmSys.ContGenPrimitives
open ClmSys.ClmErrors
open ClmSys.SolverRunnerPrimitives
open ContGenServiceInfo.ServiceInfo
open ClmSys.SolverRunnerErrors
open ClmSys.GeneralPrimitives

module SolverRunner =

    type SolverUpdateProxy =
        {
            updateProgress : ProgressUpdateInfo -> UnitResult
            updateTime : double -> double[] -> UnitResult
            checkCancellation : RunQueueId -> CancellationType option
        }


    type SolverRunnerProxy =
        {
            solverUpdateProxy : SolverUpdateProxy
            saveResult : ResultDataWithId -> UnitResult
            saveCharts : ChartGenerationResult -> UnitResult
            logCrit : SolverRunnerCriticalError -> UnitResult
        }
