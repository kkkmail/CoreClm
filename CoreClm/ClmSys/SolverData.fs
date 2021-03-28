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
