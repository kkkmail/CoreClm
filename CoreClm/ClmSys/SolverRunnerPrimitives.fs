namespace ClmSys

module SolverRunnerPrimitives =

    let defaultNoOfOutputPoints = 1000
    let defaultNoOfProgressPoints = 100


    type ResultNotificationType =
        | RegularChartGeneration
        | ForceChartGeneration


    type CancellationType =
        | CancelWithResults of string option
        | AbortCalculation of string option
