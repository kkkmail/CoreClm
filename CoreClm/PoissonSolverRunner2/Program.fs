namespace Softellect.Samples.DistrProc.SolverRunner

open System.IO
open FredholmSolver.Common
open FredholmSolver.Solver
open FredholmSolver.PoissonSolver2
open Softellect.Sys.ExitErrorCodes
open Softellect.DistributedProcessing.SolverRunner.Implementation
open Softellect.DistributedProcessing.SolverRunner.Program
open Softellect.DistributedProcessing.Primitives.Common
open Softellect.DistributedProcessing.SolverRunner.Primitives
open Softellect.Sys.Logging
open Softellect.Sys.Primitives
open Plotly.NET
open FredholmSolver.EeInfIntModel2
open Softellect.Analytics.Primitives
open Analytics.ChartExt

module Program =

    /// Set it to true to generate the animation. This will slow down the solver.
    let generateAnimation = true


    let getEeData _ (c1 : ResultSliceData<PoissonChartData> list) (t : list<double>) =
        let data =
            [
                { dataLabel = "ee mean" |> DataLabel; dataPoints = c1 |> List.mapi (fun j e -> { x = t[j]; y = e.resultData.statData.eeStatData.mean} ) }
                { dataLabel = "ee stdDev" |> DataLabel; dataPoints = c1 |> List.mapi (fun j e -> { x = t[j]; y = e.resultData.statData.eeStatData.stdDev} ) }
            ]
        Some data


    let getInfData _ (c1 : ResultSliceData<PoissonChartData> list) (t : list<double>) =
        let data =
            [
                { dataLabel = "inf mean" |> DataLabel; dataPoints = c1 |> List.mapi (fun j e -> { x = t[j]; y = e.resultData.statData.infStatData.mean} ) }
                { dataLabel = "inf stdDev" |> DataLabel; dataPoints = c1 |> List.mapi (fun j e -> { x = t[j]; y = e.resultData.statData.infStatData.stdDev} ) }
            ]
        Some data


    let getGammaData (d : PoissonSolverData) _ _ =
        let g = d.model.intModelParams.eeInfModelParams.gammaFuncValue
        let domain2D = d.model.domain2D

        match g with
        | FredholmSolver.Kernel.GammaFuncValue.SphericalGamma v ->
            Logger.logTrace (fun () -> $"getGammaData - SphericalGamma: %A{v}.")

            let dataPoints =
                domain2D.d0.points.value
                |> List.ofArray
                |> List.map (fun a -> { x = a; y = (g.gammaFunc domain2D).invoke domain2D a 0.0 })

            Logger.logTrace (fun () -> $"getGammaData - SphericalGamma, dataPoints: %A{dataPoints}")
            Some [ { dataLabel = "gamma" |> DataLabel; dataPoints = dataPoints }  ]
        | _ ->
            Logger.logWarn $"getGammaData - Cannot get data for: %A{g}."
            None


    let getKaData (d : PoissonSolverData) _ _ =
        let k = d.model.intModelParams.eeInfModelParams.kernelParams.kaFuncValue
        let domain2D = d.model.domain2D

        match d.model.intModelParams.eeInfModelParams.kernelParams.kaFuncValue with
        | KaFuncValue.SphericalKa v ->
            Logger.logTrace (fun () -> $"getKaData - SphericalKa: %A{v}.")

            let dataPoints =
                domain2D.d0.points.value
                |> List.ofArray
                |> List.map (fun a -> { x = a; y = (k.kaFunc domain2D).invoke domain2D a 0.0 })

            Logger.logTrace (fun () -> $"getKaData - SphericalKa, dataPoints: %A{dataPoints}")
            Some [ { dataLabel = "ka" |> DataLabel; dataPoints = dataPoints } ]
        | _ ->
            Logger.logWarn $"getKaData - Cannot get data for: %A{k}."
            None


    let getWolframCharts (d : PoissonSolverData) c =
        [
            getWolframChart poissonSolverId d d.fullName c FileSuffix.EeSuffix getEeData
            getWolframChart poissonSolverId d d.fullName c FileSuffix.InfSuffix getInfData
            getWolframChart poissonSolverId d d.fullName c FileSuffix.GammaSuffix getGammaData
            getWolframChart poissonSolverId d d.fullName c FileSuffix.KaSuffix getKaData
        ]
        |> List.choose id


    let getHtmlCharts _ (d : PoissonSolverData) (c : list<ResultSliceData<PoissonChartData>>) =
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


    let getCharts (q : RunQueueId) (d : PoissonSolverData) (c : list<ResultSliceData<PoissonChartData>>) =
        printfn $"getChart - q: '%A{q}', c.Length: '%A{c.Length}'."

        getHtmlCharts q d c
        @
        getWolframCharts d c
        |> List.choose id
        |> Some


    let getAnimation (d : PoissonSolverData) =
        try
            match outputAnimation d.model d.initialData with
            | Ok o ->
                if File.Exists(o.value) then
                    {
                        binaryContent = File.ReadAllBytes(o.value)
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
        let a =
            match generateAnimation with
            | true -> getAnimation d
            | false -> None

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
            Logger.logTrace (fun () -> $"generateDetailedResults - q: %A{q}, i: %A{i}, noOfEpochs: %A{noOfEpochs}, frameMod: %A{frameMod}.")

            match frameMod with
            | Some v ->
                if i % v = 0
                then
                    Logger.logTrace (fun () -> $"generateDetailedResults (outputting frame) - q: %A{q}, i: %A{i}, noOfEpochs: %A{noOfEpochs}, frameMod: %A{frameMod}.")
                    // outputFrameData d.model d.initialData x i
                    match generateAnimation with
                    | true -> outputFramePngData d.model d.initialData x i
                    | false -> ()
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
