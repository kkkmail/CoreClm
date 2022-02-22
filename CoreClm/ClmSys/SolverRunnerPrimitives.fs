namespace ClmSys

open ClmSys.GeneralPrimitives

module SolverRunnerPrimitives =

    let defaultNoOfOutputPoints = 1000
    let defaultNoOfProgressPoints = 100


    type ResultNotificationType =
        | RegularChartGeneration
        | ForceChartGeneration

        member n.value =
            match n with
            | RegularChartGeneration -> 1
            | ForceChartGeneration -> 2


    type CancellationType =
        | CancelWithResults of string option
        | AbortCalculation of string option

        member n.value =
            match n with
            | AbortCalculation _ -> 0
            | CancelWithResults _ -> 2


    type ProcessId =
        | ProcessId of int


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
            progress : decimal
            callCount : int64
            yRelative : double
            eeData : EeData
            errorMessageOpt : ErrorMessage option
        }

        static member defaultValue =
            {
                progress = 0.0m
                callCount = 0L
                yRelative = 1.0
                eeData = EeData.defaultValue
                errorMessageOpt = None
            }

