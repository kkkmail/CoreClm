namespace Softellect.Samples.DistrProc.SolverRunner

open System
open System.IO
open FredholmSolver.PoissonSolver
open Softellect.Sys.ExitErrorCodes
open Softellect.DistributedProcessing.SolverRunner.Program
open Softellect.DistributedProcessing.Primitives.Common
//open Softellect.Samples.DistrProc.Core.Primitives
open Softellect.DistributedProcessing.SolverRunner.Primitives
open Softellect.DistributedProcessing.SolverRunner.OdeSolver
open Softellect.Sys.Primitives
open Softellect.Sys.Core
//open Wolfram.NETLink
open Plotly.NET
open Giraffe.ViewEngine
open Softellect.Analytics.Wolfram
open FredholmSolver.EeInfIntModel
open Softellect.Analytics.Wolfram
open Softellect.Analytics.AppSettings
open Softellect.Analytics.Primitives

module Program =

    type ChartDescription =
        {
            Heading : string
            Text : string
        }


    let toDescription h t =
        {
            Heading = h
            Text = t
        }


    let toEmbeddedHtmlWithDescription (description : ChartDescription) (gChart : GenericChart) =
        let plotlyRef = PlotlyJSReference.Full

        let displayOpts =
            DisplayOptions.init(
                AdditionalHeadTags = [
                    script [_src description.Heading] []
                ],
                // Description = [
                //     h1 [] [str description.Heading]
                //     h2 [] [str description.Text]
                // ],
                PlotlyJSReference = plotlyRef
            )

        let result =
            gChart
            |> Chart.withDisplayOptions(displayOpts)
            |> GenericChart.toEmbeddedHTML

        result


    let toHtmlFileName (FileName fileName) =
        if fileName.EndsWith(".html", StringComparison.OrdinalIgnoreCase) then fileName
        else fileName + ".html"
        |> FileName


    let getHtmlChart fileName d ch =
        {
            textContent = toEmbeddedHtmlWithDescription d ch
            fileName = toHtmlFileName fileName
        }
        |> TextResult


    let tryGetInputFileName inputFolder (q : RunQueueId) = (FileName $"{q.value}.m").tryGetFullFileName (Some inputFolder)
    let tryGetOutputFileName outputFolder (q : RunQueueId) = (FileName $"{q.value}.png").tryGetFullFileName (Some outputFolder)


    let getWolframChart (q : RunQueueId) (d : PoissonSolverData) (c : list<ResultSliceData<PoissonChartData>>) =
        let w = loadWolframParams()

        match tryGetInputFileName w.wolframInputFolder q, tryGetOutputFileName w.wolframOutputFolder q with
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
            printfn $"getWolframChart - Cannot get data for: %A{q}."
            None

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
            getWolframChart q d c
        ]
        |> List.choose id
        |> Some


    [<EntryPoint>]
    let main argv =
        let retVal =
            try
                let chartGenerator : ResultGenerator<PoissonSolverData, SubstanceIntData, PoissonChartData> =
                    {
                        getResultData = fun d t x ->
                            let chartMod = d.initialData.evolutionParam.noOfCharts |> Option.bind (fun v -> d.initialData.evolutionParam.noOfEpochs.value / v |> Some)
                            getChartSliceData d.model d.initialData.evolutionParam.noOfEpochs chartMod x (int t.value)
                        generateResults = fun q d _ c -> getCharts q d c
                        generateDetailedResults = fun _ _ _ _ -> None
                    }

                let getUserProxy (solverData : PoissonSolverData) : SolverRunnerUserProxy<PoissonSolverData, PoissonProgressData, SubstanceIntData, PoissonChartData> =
                    let solverRunner = poissonSolverRunner solverData

                    let solverProxy : SolverProxy<PoissonSolverData, PoissonProgressData, SubstanceIntData> =
                        {
                            getInitialData = _.getInitialData()
                            getProgressData = None
                            getInvariant = fun d _ x ->
                                (double (d.model.invariant x)) / (double d.model.intModelParams.intInitParams.totalMolecules.value) |> RelativeInvariant
                        }

                    {
                        solverRunner = solverRunner
                        solverProxy = solverProxy
                        resultGenerator = chartGenerator
                    }

                // Call solverRunnerMain<'D, 'P, 'X, 'C>
                printfn "Calling solverRunnerMain..."
                solverRunnerMain<PoissonSolverData, PoissonProgressData, SubstanceIntData, PoissonChartData> poissonSolverId getUserProxy argv
            with
            | e ->
                Console.WriteLine($"Exception: %A{e}.")
                CriticalError

        // Console.ReadLine() |> ignore
        retVal
