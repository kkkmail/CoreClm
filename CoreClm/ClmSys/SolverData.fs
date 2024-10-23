namespace ClmSys

open System
open ClmSys.ContGenPrimitives
open ClmSys.SolverRunnerPrimitives
open Softellect.DistributedProcessing.Primitives.Common
//open Primitives.SolverPrimitives

module SolverData =

    type EarlyExitCheckFrequency =
        | EarlyExitCheckFrequency of TimeSpan

        member this.value = let (EarlyExitCheckFrequency v) = this in v

        static member defaultValue = TimeSpan.FromHours(1.0) |> EarlyExitCheckFrequency


    /// Collection of "standard" early exit parameters.
    type EarlyExitParam =
        {
            earlyExitCheckFreq : EarlyExitCheckFrequency
            quickProgress : decimal
            quickMinEe : double
            standardProgress : decimal
            standardMinEe : double
            slowProgress : decimal
            slowMinEe : double
            maxRunTime : TimeSpan
        }

        static member defaultValue =
            {
                earlyExitCheckFreq = EarlyExitCheckFrequency.defaultValue
                quickProgress = 0.01M
                quickMinEe = 0.30
                standardProgress = 0.05M
                standardMinEe = 0.20
                slowProgress = 0.10M
                slowMinEe = 0.15
                maxRunTime = TimeSpan.FromDays 20.0
            }


    type RunnerControlData =
        {
            minUsefulEe : MinUsefulEe
            noOfProgressPoints : int
            earlyExitParamOpt : EarlyExitParam option
            absoluteTolerance : AbsoluteTolerance
        }
