namespace GenericOdeSolverPrimitives

open System
// open ClmSys.ContGenPrimitives
// open Softellect.OdePackInterop
open Microsoft.FSharp.Core
// open Clm.ChartData
// open ClmSys.GeneralPrimitives
// open ClmSys.GeneralData
// open ClmSys.SolverRunnerPrimitives
// open ClmSys.ClmErrors
// open ClmSys.SolverData
// open Clm.CalculationData

// module SolverPrimitives =
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


    type ErrorMessage =
        | ErrorMessage of string

        member this.value = let (ErrorMessage v) = this in v


    // type EeData =
    //     {
    //         maxEe : double
    //         maxAverageEe : double
    //         maxWeightedAverageAbsEe : double
    //         maxLastEe : double
    //     }
    //
    //     static member defaultValue =
    //         {
    //             maxEe = 0.0
    //             maxAverageEe = 0.0
    //             maxWeightedAverageAbsEe = 0.0
    //             maxLastEe = 0.0
    //         }


    type ProgressData<'PD> =
        {
            progress : decimal
            callCount : int64
            yRelative : double
            progressData : 'PD
            errorMessageOpt : ErrorMessage option
        }

        static member defaultValue pd =
            {
                progress = 0.0m
                callCount = 0L
                yRelative = 1.0
                progressData = pd
                errorMessageOpt = None
            }
