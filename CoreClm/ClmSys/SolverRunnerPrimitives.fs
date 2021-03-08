namespace ClmSys

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
