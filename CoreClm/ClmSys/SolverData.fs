namespace ClmSys

open System
open ClmSys.GeneralPrimitives
open ClmSys.SolverRunnerPrimitives

module SolverData =

    let estimateEndTime progress (started : DateTime) =
        if progress >= 0.0 && progress <= 1.0
        then
            let estRunTime = (decimal (DateTime.Now.Subtract(started).Ticks)) / (decimal progress) |> int64 |> TimeSpan.FromTicks
            started.Add estRunTime |> Some
        else None


    type ProgressData
        with

        member data.estimateEndTime (started : DateTime) = estimateEndTime data.progress started
        
        
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
