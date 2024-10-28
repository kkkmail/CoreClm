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
//open Plotly.NET
//open Giraffe.ViewEngine
open Softellect.Sys.Wolfram
open FredholmSolver.EeInfIntModel

module Program =

    //type ChartDescription =
    //    {
    //        Heading : string
    //        Text : string
    //    }

    //type WolframRequest =
    //    {
    //        content : string // A content of .m file to be put into inputFolder\inputFileName
    //        inputFolder : string
    //        inputFileName : string
    //        outputFolder : string
    //        outputFileName : string
    //        extension : string
    //    }


    //// Function to ensure a directory exists and create it if it doesn't
    //let ensureDirectoryExists dir =
    //    if not (Directory.Exists dir) then
    //        Directory.CreateDirectory dir |> ignore


    //let runMathematicaScript (request: WolframRequest) =
    //    try
    //        // Ensure input and output folders exist
    //        ensureDirectoryExists request.inputFolder
    //        ensureDirectoryExists request.outputFolder

    //        // Write the content to the input file
    //        let inputFilePath = Path.Combine(request.inputFolder, request.inputFileName)
    //        File.WriteAllText(inputFilePath, request.content)

    //       // Start the Wolfram Kernel with explicit link name
    //        let linkArgs = "-linkname 'C:\\Program Files\\Wolfram Research\\Mathematica\\13.0\\mathkernel.exe -mathlink'"
    //        let link = MathLinkFactory.CreateKernelLink(linkArgs)

    //        try
    //            // Discard the initial kernel output
    //            link.WaitAndDiscardAnswer()

    //            // Load the .m file as a script and run it
    //            let scriptCommand = $"<< \"%s{inputFilePath}\""  // Use "<< file.m" to load the script
    //            link.Evaluate(scriptCommand)

    //            // Wait for the result of the evaluation
    //            link.WaitForAnswer() |> ignore

    //            // Check for the output file in the output folder
    //            let outputFilePath = Path.Combine(request.outputFolder, request.outputFileName) + "." + request.extension
    //            if File.Exists(outputFilePath) then
    //                // If the output file is found, read it as a byte array and return it as Ok
    //                let fileBytes = File.ReadAllBytes(outputFilePath)
    //                link.Close() // Close the link when done
    //                Ok fileBytes
    //            else
    //                // If the output file is not found, return an error
    //                link.Close()
    //                Error $"Output file '{outputFilePath}' not found."

    //        with
    //        | ex ->
    //            link.Close()
    //            Error $"An error occurred during Wolfram evaluation: {ex.Message}"

    //    with
    //    | ex -> Error $"An error occurred: {ex.Message}"


    //let toDescription h t =
    //    {
    //        Heading = h
    //        Text = t
    //    }


    //let toEmbeddedHtmlWithDescription (description : ChartDescription) (gChart : GenericChart) =
    //    let plotlyRef = PlotlyJSReference.Full

    //    let displayOpts =
    //        DisplayOptions.init(
    //            AdditionalHeadTags = [
    //                script [_src description.Heading] []
    //            ],
    //            // Description = [
    //            //     h1 [] [str description.Heading]
    //            //     h2 [] [str description.Text]
    //            // ],
    //            PlotlyJSReference = plotlyRef
    //        )

    //    let result =
    //        gChart
    //        |> Chart.withDisplayOptions(displayOpts)
    //        |> GenericChart.toEmbeddedHTML

    //    result


    //let toHtmlFileName (FileName fileName) =
    //    if fileName.EndsWith(".html", StringComparison.OrdinalIgnoreCase) then fileName
    //    else fileName + ".html"
    //    |> FileName


    //let getHtmlChart fileName d ch =
    //    {
    //        htmlContent = toEmbeddedHtmlWithDescription d ch
    //        fileName = toHtmlFileName fileName
    //    }
    //    |> HtmlChart


    //let inputFolder = "C:\\\\Temp\\\\WolframInput\\\\"
    //let outputFolder = "C:\\\\Temp\\\\WolframOutput\\\\"
    //let getInputFileName (q : RunQueueId) = $"{q.value}.m"
    //let getOutputFileName (q : RunQueueId) = $"{q.value}"


    //let getWolframData (q : RunQueueId) (d : TestSolverData) (c : list<ChartSliceData<TestChartData>>) =
    //    let c1 = c |>  List.rev

    //    match c1 |> List.tryHead with
    //    | Some h ->
    //        let t = c1 |> List.map(fun e -> double e.t)
    //        let legends = d.chartLabels

    //        let x =
    //            h.chartData.x
    //            |> Array.mapi (fun i  _ -> c1 |> List.map (fun e -> e.chartData.x[i]))

    //        let xValues = x |> Array.mapi(fun i e -> $"x{i} = {(toWolframNotation e)};") |> List.ofArray
    //        let txValues = x |> Array.mapi(fun i e -> $"tx{i} = Table[{{t[[i]], x{i}[[i]]}}, {{i, 1, Length[t]}}];") |> List.ofArray
    //        let txVar = x |> Array.mapi(fun i _ -> $"tx{i}") |> joinStrings ", "

    //        let data =
    //            [
    //                $"t = {(toWolframNotation t)};"
    //            ]
    //            @
    //            xValues
    //            @
    //            txValues
    //            @
    //            [
    //                $"legends = {(toWolframNotation legends)};"
    //                $"outputFile = \"{outputFolder}{(getOutputFileName q)}.png\";"
    //                $"Export[outputFile, ListLinePlot[{{{txVar}}}, Frame -> True, PlotRange -> All, GridLines -> Automatic, PlotLegends -> legends], \"PNG\"];"
    //            ]
    //            |> joinStrings Nl

    //        Some data
    //    | None -> None


    //let getWolframChart (q : RunQueueId) (d : TestSolverData) (c : list<ChartSliceData<TestChartData>>) =
    //    try
    //        match getWolframData q d c with
    //        | Some data ->
    //            let request =
    //                {
    //                    content = data
    //                    inputFolder = inputFolder
    //                    inputFileName = getInputFileName q
    //                    outputFolder = outputFolder
    //                    outputFileName = getOutputFileName q
    //                    extension = "png"
    //                }

    //            match runMathematicaScript request with
    //            | Ok v ->
    //                {
    //                    binaryContent = v
    //                    fileName = FileName (request.outputFileName + "." + request.extension)
    //                }
    //                |> BinaryChart
    //                |> Some
    //            | Error e ->
    //                printfn $"getWolframChart - Error: %A{e}."
    //                None
    //        | None ->
    //            printfn $"getWolframChart - Cannot get data for: %A{q}."
    //            None
    //    with
    //    | e ->
    //        printfn $"getWolframChart - Exception: %A{e}."
    //        None


    //let getCharts (q : RunQueueId) (d : TestSolverData) (c : list<ChartSliceData<TestChartData>>) =
    //    printfn $"getChart - q: '%A{q}', c.Length: '%A{c.Length}'."

    //    let charts =
    //        match c |> List.tryHead with
    //        | Some h ->
    //            h.chartData.x
    //            |> Array.mapi (fun i  _ -> Chart.Line(c |> List.map (fun c -> c.t, c.chartData.x[i]), Name = d.chartLabels[i]))
    //        | None -> [||]

    //    let chart = Chart.combine charts

    //    [
    //        getHtmlChart (FileName $"{q.value}") (toDescription "Heading" "Text") chart |> Some
    //        getWolframChart q d c
    //    ]
    //    |> List.choose id
    //    |> Some


    [<EntryPoint>]
    let main argv =
        let retVal =
            try
                let chartGenerator : ChartGenerator<PoissonSolverData, SubstanceIntData, PoissonChartData> =
                    {
                        getChartData = fun d t x ->
                            let chartMod = d.initialData.evolutionParam.noOfCharts |> Option.bind (fun v -> d.initialData.evolutionParam.noOfEpochs.value / v |> Some)
                            getChartSliceData d.model d.initialData.evolutionParam.noOfEpochs chartMod x (int t.value)
                        generateCharts = fun q d _ c -> None //getCharts q d c
                        generateDetailedCharts = fun _ _ _ _ -> None
                    }

                let getUserProxy (solverData : PoissonSolverData) : SolverRunnerUserProxy<PoissonSolverData, PoissonProgressData, SubstanceIntData, PoissonChartData> =
                    let solverRunner = poissonSolverRunner solverData

                    let solverProxy : SolverProxy<PoissonSolverData, PoissonProgressData, SubstanceIntData> =
                        {
                            getInitialData = _.getInitialData()
                            getProgressData = None
                            getInvariant = fun _ _ _ -> RelativeInvariant 1.0
                        }

                    {
                        solverRunner = solverRunner
                        solverProxy = solverProxy
                        chartGenerator = chartGenerator
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
