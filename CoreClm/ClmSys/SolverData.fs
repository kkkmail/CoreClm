namespace ClmSys

open System
open ClmSys.GeneralPrimitives

module SolverData =

    let estimateEndTime progress (started : DateTime) =
        if progress >= 0.0 && progress <= 1.0
        then
            let estRunTime = (decimal (DateTime.Now.Subtract(started).Ticks)) / (decimal progress) |> int64 |> TimeSpan.FromTicks
            started.Add estRunTime |> Some
        else None


    type EeData =
        {
            maxEe : double
            maxAverageEe : double
            maxWeightedAverageAbsEe : double
            maxLastEe : double
        }

        static member defaultValue =
            {
                maxEe = 0.0
                maxAverageEe = 0.0
                maxWeightedAverageAbsEe = 0.0
                maxLastEe = 0.0
            }


    type ProgressData =
        {
            progress : double
            callCount : int64
            yRelative : double
            eeData : EeData
            errorMessageOpt : ErrorMessage option
        }

        static member defaultValue =
            {
                progress = 0.0
                callCount = 0L
                yRelative = 1.0
                eeData = EeData.defaultValue
                errorMessageOpt = None
            }

        member data.estimateEndTime (started : DateTime) = estimateEndTime data.progress started
