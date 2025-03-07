﻿namespace ClmSys

open System
open Softellect.DistributedProcessing.Primitives.Common
//open ClmSys.GeneralPrimitives
//open Primitives.GeneralPrimitives
//open Primitives.SolverPrimitives

module SolverRunnerPrimitives =

    let clmSolverId = "5FA3E38A-87A9-49C5-8A85-5A5AC4A71CAC" |> Guid.Parse |> SolverId
    let clmSolverName = "Clm" |> SolverName

//     let defaultNoOfOutputPoints = 1000
//     let defaultNoOfProgressPoints = 100
//
//
//     type ResultNotificationType =
//         | RegularChartGeneration
//         | ForceChartGeneration
//
//         member n.value =
//             match n with
//             | RegularChartGeneration -> 1
//             | ForceChartGeneration -> 2
//
//
//     type CancellationType =
//         | CancelWithResults of string option
//         | AbortCalculation of string option
//
//         member n.value =
//             match n with
//             | AbortCalculation _ -> 0
//             | CancelWithResults _ -> 2
//
//
//     type ProcessId =
//         | ProcessId of int
//
//
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


    // type ProgressData =
    //     {
    //         progress : decimal
    //         callCount : int64
    //         yRelative : double
    //         eeData : EeData
    //         errorMessageOpt : ErrorMessage option
    //     }
    //
    //     static member defaultValue =
    //         {
    //             progress = 0.0m
    //             callCount = 0L
    //             yRelative = 1.0
    //             eeData = EeData.defaultValue
    //             errorMessageOpt = None
    //         }
    //
    //     member data.estimateEndTime (started : DateTime) = estimateEndTime data.progress started


    // type ClmProgressAdditionalData =
    //     {
    //         yRelative : double
    //         eeData : EeData
    //     }
    //
    //     static member defaultValue =
    //         {
    //             yRelative = 1.0
    //             eeData = EeData.defaultValue
    //         }

    // type ClmProgressData = ProgressData<ClmProgressAdditionalData>


    //type ClmProgressData =
    //    {
    //        //progressData : ProgressData
    //        yRelative : double
    //        eeData : EeData
    //    }

    //    static member defaultValue =
    //        {
    //            //progressData = ProgressData.defaultValue
    //            yRelative = 1.0
    //            eeData = EeData.defaultValue
    //        }

    //    //member data.estimateEndTime (started : DateTime) = estimateEndTime data.progressData.progress started
