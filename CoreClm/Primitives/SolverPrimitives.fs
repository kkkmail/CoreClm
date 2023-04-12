namespace Primitives

open System
open Microsoft.FSharp.Core
open Primitives.GeneralPrimitives

module SolverPrimitives =

    [<Literal>]
    let DefaultAbsoluteTolerance = 1.0e-08

    let defaultNoOfOutputPoints = 1000
    let defaultNoOfProgressPoints = 100


    type AbsoluteTolerance =
        | AbsoluteTolerance of double

        member this.value = let (AbsoluteTolerance v) = this in v
        static member defaultValue = AbsoluteTolerance DefaultAbsoluteTolerance


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


    // type ErrorMessage =
    //     | ErrorMessage of string
    //
    //     member this.value = let (ErrorMessage v) = this in v


    let estimateEndTime progress (started : DateTime) =
        if progress > 0.0m && progress <= 1.0m
        then
            let estRunTime = (decimal (DateTime.Now.Subtract(started).Ticks)) / progress |> int64 |> TimeSpan.FromTicks
            started.Add estRunTime |> Some
        else None


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

        member data.estimateEndTime (started : DateTime) = estimateEndTime data.progress started
