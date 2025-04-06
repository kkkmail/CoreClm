namespace Softellect.Samples.DistrProc.SolverRunner

open System.IO
open FredholmSolver.Solver
open FredholmSolver.PoissonSolver2
open Softellect.Sys.ExitErrorCodes
open Softellect.DistributedProcessing.SolverRunner.Implementation
open Softellect.DistributedProcessing.SolverRunner.Program
open Softellect.DistributedProcessing.Primitives.Common
open Softellect.DistributedProcessing.SolverRunner.Primitives
open Softellect.Sys.Logging
open Softellect.Sys.Primitives
open Softellect.Sys.Core
open Plotly.NET
open Softellect.Analytics.Wolfram
open FredholmSolver.EeInfIntModel2
open Softellect.Analytics.AppSettings
open Softellect.Analytics.Primitives
open Analytics.ChartExt

module Program =

    let tryGetInputEeFileName inputFolder (d : PoissonSolverData) = (FileName $"{d.fullName}__ee.m").tryGetFullFileName (Some inputFolder)
    let tryGetOutputEeFileName outputFolder (d : PoissonSolverData) = (FileName $"{d.fullName}__ee.png").tryGetFullFileName (Some outputFolder)

    let tryGetInputInfFileName inputFolder (d : PoissonSolverData) = (FileName $"{d.fullName}__inf.m").tryGetFullFileName (Some inputFolder)
    let tryGetOutputInfFileName outputFolder (d : PoissonSolverData) = (FileName $"{d.fullName}__inf.png").tryGetFullFileName (Some outputFolder)


    let getWolframEeChart (q : RunQueueId) (d : PoissonSolverData) (c : list<ResultSliceData<PoissonChartData>>) =
        let w = getSolverWolframParams poissonSolverId

        match tryGetInputEeFileName w.wolframInputFolder d, tryGetOutputEeFileName w.wolframOutputFolder d with
        | Ok i, Ok o ->
            let c1 = c |> List.rev
            let t = c1 |> List.map(fun e -> double e.t)

            let d =
                [|
                    { dataLabel = "ee mean" |> DataLabel; dataPoints = c1 |> List.mapi (fun j e -> { x = t[j]; y = e.resultData.statData.eeStatData.mean} ) }
                    { dataLabel = "ee stdDev" |> DataLabel; dataPoints = c1 |> List.mapi (fun j e -> { x = t[j]; y = e.resultData.statData.eeStatData.stdDev} ) }
                |]

            getListLinePlot i o ListLineParams.defaultValue d
        | _ ->
            Logger.logError $"getWolframEeChart - Cannot get data for: %A{q}."
            None


    let getWolframInfChart (q : RunQueueId) (d : PoissonSolverData) (c : list<ResultSliceData<PoissonChartData>>) =
        let w = getSolverWolframParams poissonSolverId

        match tryGetInputInfFileName w.wolframInputFolder d, tryGetOutputInfFileName w.wolframOutputFolder d with
        | Ok i, Ok o ->
            let c1 = c |> List.rev
            let t = c1 |> List.map(fun e -> double e.t)

            let d =
                [|
                    { dataLabel = "inf mean" |> DataLabel; dataPoints = c1 |> List.mapi (fun j e -> { x = t[j]; y = e.resultData.statData.infStatData.mean} ) }
                    { dataLabel = "inf stdDev" |> DataLabel; dataPoints = c1 |> List.mapi (fun j e -> { x = t[j]; y = e.resultData.statData.infStatData.stdDev} ) }
                |]

            getListLinePlot i o ListLineParams.defaultValue d
        | _ ->
            Logger.logError $"getWolframInfChart - Cannot get data for: %A{q}."
            None


    let getWolframCharts q d c =
        [
            getWolframEeChart q d c
            getWolframInfChart q d c
        ]


    let getCharts (q : RunQueueId) (d : PoissonSolverData) (c : list<ResultSliceData<PoissonChartData>>) =
        printfn $"getChart - q: '%A{q}', c.Length: '%A{c.Length}'."

        let charts =
            match c |> List.tryHead with
            | Some _ ->
                [|
                    Chart.Line(c |> List.map (fun e -> e.t, d.norm * (double e.resultData.statData.invariant)), Name = "Invariant")
                    Chart.Line(c |> List.map (fun e -> e.t, d.norm * (double e.resultData.statData.food)), Name = "Food")
                    Chart.Line(c |> List.map (fun e -> e.t, d.norm * (double e.resultData.statData.waste)), Name = "Waste")
                    Chart.Line(c |> List.map (fun e -> e.t, d.norm * (double e.resultData.statData.total)), Name = "Total")
                |]
            | None -> [||]

        let chart = Chart.combine charts

        [
            getHtmlChart (FileName $"{d.fullName}__subst") (toDescription "Heading" "Text") chart |> Some
        ]
        @ (getWolframCharts q d c)
        |> List.choose id
        |> Some


    let getAnimation (d : PoissonSolverData) =
        try
            match outputAnimation d.model d.initialData with
            | Ok o ->
                if File.Exists(o.value) then
                    let v = File.ReadAllBytes(o.value)
                    {
                        binaryContent = v
                        fileName = o
                    }
                    |> BinaryResult
                    |> Some
                else None
            | Error e ->
                Logger.logCrit($"Error: %A{e}.")
                None
        with
        | e ->
            Logger.logCrit($"Exception: %A{e}.")
            None


    let getAllResults (q : RunQueueId) (d : PoissonSolverData) (c : list<ResultSliceData<PoissonChartData>>) =
        let a = getAnimation d
        let c = getCharts q d c

        match a, c with
        | Some a1, Some c1 -> a1 :: c1 |> Some
        | Some a1, None -> [ a1 ] |> Some
        | None, Some c1 -> c1 |> Some
        | None, None -> None


    let generateDetailedResults (q : RunQueueId) (d : PoissonSolverData) (t : EvolutionTime) (x : SubstanceData) =
        try
            // Evolution time is effectively measured in ints.
            let i = int t.value
            let noOfEpochs = d.initialData.evolutionParam.noOfEpochs.value
            let frameMod = d.initialData.evolutionParam.noOfFrames |> Option.bind (fun v ->  max (noOfEpochs / v) 1 |> Some)
            // let chartFrequency = if noOfEpochs <= d.initialData.evolutionParam.maxChartPoints then 1 else noOfEpochs / d.initialData.evolutionParam.maxChartPoints
            Logger.logTrace $"generateDetailedResults - q: %A{q}, i: %A{i}, noOfEpochs: %A{noOfEpochs}, frameMod: %A{frameMod}."

            match frameMod with
            | Some v ->
                if i % v = 0
                then
                    Logger.logTrace $"generateDetailedResults (outputting frame) - q: %A{q}, i: %A{i}, noOfEpochs: %A{noOfEpochs}, frameMod: %A{frameMod}."
                    // outputFrameData d.model d.initialData x i
                    outputFramePngData d.model d.initialData x i
            | None -> ()
        with
        | e -> Logger.logCrit($"Exception: %A{e}.")

        // We don't want to accumulate the data as we output it during the evolution.
        None


    [<EntryPoint>]
    let main argv =
        let retVal =
            try
                let chartGenerator : ResultGenerator<PoissonSolverData, SubstanceData, PoissonChartData> =
                    {
                        getResultData = fun d t x ->
                            let chartMod = d.initialData.evolutionParam.noOfCharts |> Option.bind (fun v -> d.initialData.evolutionParam.noOfEpochs.value / v |> Some)
                            getChartSliceData d.model d.initialData.evolutionParam.noOfEpochs chartMod x (int t.value)
                        // generateResults = fun q d _ c -> getCharts q d c
                        generateResults = fun q d _ c -> getAllResults q d c
                        generateDetailedResults = generateDetailedResults
                    }

                let solverProxy : SolverProxy<PoissonSolverData, PoissonProgressData, SubstanceData> =
                    {
                        getInitialData = _.getInitialData()
                        getProgressData = None
                        getInvariant = fun d _ x ->
                            (double (d.model.invariant x)) / (double d.model.intModelParams.intInitParams.totalMolecules.value) |> RelativeInvariant
                        getOptionalFolder = fun _ _ -> None
                    }

                let getUserProxy (solverData : PoissonSolverData) : SolverRunnerUserProxy<PoissonSolverData, PoissonProgressData, SubstanceData, PoissonChartData> =
                    let solverRunner = poissonSolverRunner solverData

                    {
                        solverRunner = solverRunner
                        solverProxy = solverProxy
                        resultGenerator = chartGenerator
                    }

                // Call solverRunnerMain<'D, 'P, 'X, 'C>
                Logger.logInfo "Calling solverRunnerMain..."
                solverRunnerMain<PoissonSolverData, PoissonProgressData, SubstanceData, PoissonChartData> poissonSolverId getUserProxy argv
            with
            | e ->
                Logger.logCrit($"Exception: %A{e}.")
                CriticalError

        Logger.logInfo $"Completion code: %A{retVal}."
        // Console.ReadLine() |> ignore
        retVal
