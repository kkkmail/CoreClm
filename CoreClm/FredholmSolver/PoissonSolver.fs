namespace FredholmSolver

open System.Diagnostics
open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FredholmSolver.EeInfCharts
open Primitives.GeneralData
open System
open GenericOdeSolver.Primitives
open GenericOdeSolver.Solver
open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives
open FredholmSolver.Primitives
open FredholmSolver.Kernel
open FredholmSolver
open FredholmSolver.EeInfIntModel
open FredholmSolver.EeInfChartData
open Primitives.SolverRunnerErrors
open Softellect.Sys.Core
open Softellect.Sys.Logging
open Plotly.NET
open Analytics.ChartExt
open Primitives.ChartPrimitives
open Primitives.WolframPrimitives

module PoissonSolver =

    type PoissonEvolutionParam =
        {
            noOfEpochs : NoOfEpochs
            noOfCharts : int
            maxChartPoints : int
            name : string option
            outputFolder : string
            odePackChartSupportFolder : string
        }


    type PoissonParam =
        {
            runQueueId : RunQueueId
            intModelParams : EeInfIntModelParams
            evolutionParam : PoissonEvolutionParam
        }

        static member defaultValue mp n =
            {
                runQueueId = RunQueueId.getNewId()
                intModelParams = mp
                evolutionParam =
                    {
                        noOfEpochs = n
                        noOfCharts = 20
                        maxChartPoints = 100_000
                        name = None
                        outputFolder = @"C:\EeInf"
                        odePackChartSupportFolder = @"C:\\GitHub\\CoreClm\\Math\\odePackChartSupport.m" // Need \\ for Wolfram.
                    }
            }

        member p.modelString =
            let a = p.intModelParams.modelString
            let b = p.evolutionParam.noOfEpochs.value |> int64 |> toModelStringInt64 0L |> Option.defaultValue EmptyString
            $"{a}_{b}"

        member p.fullName = p.evolutionParam.name |> Option.defaultValue $"{p.runQueueId}"


    let createModel (mp : EeInfIntModelParams) name =
        let md = mp.named name
        let model = EeInfIntModel.create md
        model


    let getChartInitData (model : EeInfIntModel) noOfEpochs =
        {
            baseData =
                {
                    resultId = RunQueueId.getNewId()
                    modelParams = model.intModelParams.eeInfModelParams
                    domain2D = model.kernelData.domain2D
                }
            totalMolecules = model.intModelParams.intInitParams.totalMolecules
            noOfEpochs = noOfEpochs
        }


    let getChartSliceData model noOfEpochs chartMod e i : ChartSliceIntData =
        {
            epochNumber = i
            progress = (decimal i) / (decimal noOfEpochs)
            statData  = calculateIntStat model e
            substanceData  = if i % chartMod = 0 then Some e else None
        }

    let outputProgress (sw : Stopwatch) noOfEpochs progressFreq i =
        if i % progressFreq = 0 || i <= 10 then
            let t = double sw.ElapsedMilliseconds
            let dt = t / (double (i + 1))
            let estCompl = DateTime.Now.AddMilliseconds(dt * (double (noOfEpochs - i - 1))).ToString("yyyy-MM-dd HH:mm:ss")
            printfn $"Completed {i} of {noOfEpochs} steps in {(t / 60_000.0):N2} minutes. Estimated completion: {estCompl}."


    let toWolframData (model : EeInfIntModel) (p : PoissonEvolutionParam) (substanceData : SubstanceIntData) (chartData : ChartIntData) =
        let a = $"""Get["{p.odePackChartSupportFolder}"];{Nl}{Nl}"""
        let b = $"""plotAll[1];{Nl}"""
        let d = model.intModelParams |> toOutputString |> toWolframNotation
        let descr = $"descr ={Nl}\"{d}{Nl}noOfEpochs = {p.noOfEpochs}\";{Nl}{Nl}"
        let gamma0 = model.intModelParams.eeInfModelParams.gammaFuncValue.gamma0.value
        let k0 = model.intModelParams.eeInfModelParams.kernelParams.kaFuncValue.k0.value
        let totalMolecules = model.intModelParams.intInitParams.totalMolecules.value
        let norm = 100.0 / (double totalMolecules) // Use values in %.

        let eta = model.kernelData.domain2D.eeDomain.points.value
        let zeta = model.kernelData.domain2D.infDomain.points.value
        let etaData = $"etaData = {(toWolframNotation eta)};{Nl}{Nl}"
        let zetaData = $"zetaData = {(toWolframNotation zeta)};{Nl}{Nl}"
        let k0Data = $"k0 = {(toWolframNotation k0)};{Nl}{Nl}"
        let gamma0Data = $"gamma0 = {(toWolframNotation gamma0)};{Nl}{Nl}"

        let u = (norm * (substanceData.protocell.value.convert double)).value
        let uData = $"uData = {(toWolframNotation u)};{Nl}{Nl}"

        // Need to rescale ka back.
        let n = model.intModelParams.eeInfModelParams.numberOfMolecules.value
        let kMult = pown (double totalMolecules) n
        let ka = model.kernelData.ka.value.value |> Array.map (fun a -> a |> Array.map (fun b -> b * kMult / k0))

        let kaData = $"ka = {(toWolframNotation ka)};{Nl}{Nl}"

        let gamma = model.gamma.value.value |> Array.map (fun a -> a |> Array.map (fun b -> b / gamma0))
        let gammaData = $"gamma = {(toWolframNotation gamma)};{Nl}{Nl}"

        let w (e : ChartSliceIntData) =
            let g x = toWolframNotation x

            let a =
                [
                    $"{(g (int e.epochNumber))}"
                    $"{g e.statData.eeStatData.mean}"
                    $"{g e.statData.eeStatData.stdDev}"
                    $"{g e.statData.infStatData.mean}"
                    $"{g e.statData.infStatData.stdDev}"
                    $"{g (norm * (double e.statData.invariant))}"
                    $"{g (norm * (double e.statData.total))}"
                    $"{g (norm * (double e.statData.food))}"
                    $"{g (norm * (double e.statData.waste))}"
                    $"{g e.progress}"
                ]
                |> joinStrings ", "

            $"{{ {a} }}"

        let chartTitles = $"{Nl}chartTitles = {{\"epochNumber\", \"eeMean\", \"eeStdDev\", \"infMean\", \"infStdDev\", \"invariant\", \"total\", \"food\", \"waste\", \"progress\"}};{Nl}"

        let chart =
            chartData.allChartData
            |> List.sortBy (fun e -> e.epochNumber)
            |> List.map w
            |> joinStrings $",{Nl}"

        let chartDataStr = $"chartData = {{ {chart} }};{Nl}{Nl}"

        let uw (e : ChartSliceIntData) =
            match e.substanceData with
            | Some v ->
                let t = toWolframNotation (int e.epochNumber)
                let u = (norm * (v.protocell.value.convert double)).value |> toWolframNotation
                $"{{ {t}, {u} }}" |> Some
            | None -> None

        let ut =
            chartData.allChartData
            |> List.sortBy (fun e -> e.epochNumber)
            |> List.map uw
            |> List.choose id
            |> joinStrings $",{Nl}"

        let uDataT = $"uDataT = {{ {ut} }};{Nl}{Nl}"

        $"{a}{descr}{k0Data}{gamma0Data}{etaData}{zetaData}{kaData}{gammaData}{uData}{chartTitles}{chartDataStr}{uDataT}{b}"


    let runPoissonEvolution (p : PoissonParam) =
        let model = createModel p.intModelParams p.fullName
        let noOfEpochs = p.evolutionParam.noOfEpochs.value
        let progressFreq = noOfEpochs / 100
        let initialValue = model.intInitialValues
        // let startInv = model.invariant initialValue
        // let startStat = calculateIntStat model initialValue
        let ps = Random p.intModelParams.intInitParams.seedValue |> PoissonSampler.create
        let chartMod = noOfEpochs / p.evolutionParam.noOfCharts
        let chartFrequency = if noOfEpochs <= p.evolutionParam.maxChartPoints then 1 else noOfEpochs / p.evolutionParam.maxChartPoints

        let chartInitData = getChartInitData model p.evolutionParam.noOfEpochs
        let chartDataUpdater = AsyncChartIntDataUpdater(ChartIntDataUpdater(), chartInitData)
        let getChartSliceData = getChartSliceData model noOfEpochs chartMod
        let sw = Stopwatch.StartNew()

        let evolve e i =
            let e1 = model.evolve ps e
            outputProgress sw noOfEpochs progressFreq i
            if i % chartFrequency = 0 then getChartSliceData e1 i |> chartDataUpdater.addContent
            e1

        let result = [|for i in 0..noOfEpochs -> i |] |> Array.fold evolve initialValue
        // let endInv = model.invariant result
        // let endStat = calculateIntStat model result
        let chartData = chartDataUpdater.getContent()
        // chartData |> outputChart

        let wolframFileName = $@"{p.evolutionParam.outputFolder}\{p.fullName}.m"
        let wolframData = toWolframData model p.evolutionParam result chartData
        File.WriteAllText(wolframFileName, wolframData)
