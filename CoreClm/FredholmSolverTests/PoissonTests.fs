namespace FredholmSolverTests

open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FredholmSolver.EeInfCharts
open Primitives.GeneralData
open System
open FluentAssertions.Execution
open FluentAssertions
open GenericOdeSolver.Primitives
open GenericOdeSolver.Solver
open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives
open FredholmSolver.Primitives
open FredholmSolver.Kernel
open FredholmSolver.EeInfIntModel
open FredholmSolver.EeInfChartData
open Primitives.SolverRunnerErrors
open Softellect.Sys.Core
open Softellect.Sys.Logging
open Xunit
open Xunit.Abstractions
open Plotly.NET
open Analytics.ChartExt
open Primitives.ChartPrimitives
open Primitives.WolframPrimitives


type PoissonTests (output : ITestOutputHelper) =
    let writeLine s = output.WriteLine s
    let nullString : string = null
    let errInvTolerance = 1.0e-3
    let outputFolder = @"C:\EeInf"
    let noOfCharts = 20

    let createModelData mp noOfDomainPoints k0 modifier =
        let md =
            mp
            |> EeInfIntModelParams.withDomainIntervals (DomainIntervals noOfDomainPoints)
            |> EeInfIntModelParams.withK0 k0
            |> modifier

        md

    // Flat
    let mp_d100k01e01a0 = createModelData EeInfIntModelParams.defaultValue 100 K0.defaultSmallValue id

    // Quadratic in inf space
    let mp_d100k01e01g01 = createModelData EeInfIntModelParams.defaultNonLinearValue 100 K0.defaultSmallValue id
    let mp_d200k001e01g01 = createModelData EeInfIntModelParams.defaultNonLinearValue 200 K0.defaultVerySmallValue id
    let mp_d200k01e01g01 = createModelData EeInfIntModelParams.defaultNonLinearValue 200 K0.defaultSmallValue id

    // Quadratic  with small linear factor in inf space.
    let mp_d100k01e01g01i01 = createModelData EeInfIntModelParams.defaultQuadraticWithLinearInfValue 100 K0.defaultSmallValue id
    let mp_d200k001e01g01i01 = createModelData EeInfIntModelParams.defaultQuadraticWithLinearInfValue 200 K0.defaultVerySmallValue id
    let mp_d200k01e01g01i01 = createModelData EeInfIntModelParams.defaultQuadraticWithLinearInfValue 200 K0.defaultSmallValue id

    let createModel (mp : EeInfIntModelParams) name =
        let md = mp.named name
        let model = EeInfIntModel.create md
        model

    let getChartInitData model noOfEpochs =
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

    let outputChart (chartData : ChartIntData) =
        writeLine "epochNumber,food,waste,total,invariant,eeMean,eeStdDev,infMean,infStdDev"

        chartData.allChartData
        |> List.rev
        |> List.map (fun e -> writeLine $"{e.epochNumber},{e.statData.food},{e.statData.waste},{e.statData.total},{e.statData.invariant},{e.statData.eeStatData.mean},{e.statData.eeStatData.stdDev},{e.statData.infStatData.mean},{e.statData.infStatData.stdDev}")
        |> ignore

    let toWolframData (model : EeInfIntModel) noOfEpochs (substanceData : SubstanceIntData) (chartData : ChartIntData) =
        let a = $"""Get["C:\\GitHub\\CoreClm\\Math\\odePackChartSupport.m"];{Nl}{Nl}"""
        let b = $"""plotAll[1];{Nl}"""
        let d = model.intModelParams |> toOutputString |> toWolframNotation
        let descr = $"descr ={Nl}\"{d}{Nl}noOfEpochs = {noOfEpochs}\";{Nl}{Nl}"
        let gamma0 = model.intModelParams.eeInfModelParams.gammaFuncValue.gamma0.value
        let k0 = model.intModelParams.eeInfModelParams.kernelParams.kaFuncValue.k0.value

        let eta = model.kernelData.domain2D.eeDomain.points.value
        let zeta = model.kernelData.domain2D.infDomain.points.value
        let etaData = $"etaData = {(toWolframNotation eta)};{Nl}{Nl}"
        let zetaData = $"zetaData = {(toWolframNotation zeta)};{Nl}{Nl}"
        let k0Data = $"k0 = {(toWolframNotation k0)};{Nl}{Nl}"
        let gamma0Data = $"gamma0 = {(toWolframNotation gamma0)};{Nl}{Nl}"

        let u = substanceData.protocell.value
        let uData = $"uData = {(toWolframNotation u)};{Nl}{Nl}"
        let ka = model.kernelData.ka.value.value |> Array.map (fun a -> a |> Array.map (fun b -> b / k0))

        let kaData = $"ka = {(toWolframNotation ka)};{Nl}{Nl}"

        let gamma = model.gamma.value.value |> Array.map (fun a -> a |> Array.map (fun b -> b / gamma0))
        let gammaData = $"gamma = {(toWolframNotation gamma)};{Nl}{Nl}"

        let w (e : ChartSliceIntData) =
            let g x = toWolframNotation x

            let a =
                [
                    $"{(g e.epochNumber)}"
                    $"{g e.statData.eeStatData.mean}"
                    $"{g e.statData.eeStatData.stdDev}"
                    $"{g e.statData.infStatData.mean}"
                    $"{g e.statData.infStatData.stdDev}"
                    $"{g e.statData.invariant}"
                    $"{g e.statData.total}"
                    $"{g e.statData.food}"
                    $"{g e.statData.waste}"
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
                let t = toWolframNotation e.epochNumber
                let u = v.protocell.value |> toWolframNotation
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

    let runPoissonEvolution mp noOfEpochs name =
        let model = createModel mp name
        let initialValue = model.intInitialValues
        let startInv = model.invariant initialValue
        let startStat = calculateIntStat model initialValue
        let ps = Random 1 |> PoissonSampler.create
        let chartMod = noOfEpochs / noOfCharts

        let chartInitData = getChartInitData model noOfEpochs
        let chartDataUpdater = AsyncChartIntDataUpdater(ChartIntDataUpdater(), chartInitData)
        let getChartSliceData = getChartSliceData model noOfEpochs chartMod

        let evolve e i =
            let e1 = model.evolve ps e
            getChartSliceData e1 i |> chartDataUpdater.addContent
            e1

        let result = [|for i in 0..noOfEpochs -> i |] |> Array.fold evolve initialValue
        let endInv = model.invariant result
        let endStat = calculateIntStat model result
        let chartData = chartDataUpdater.getContent()
        chartData |> outputChart

        let wolframFileName = $@"{outputFolder}\{name}.m"
        let wolframData = toWolframData model noOfEpochs result chartData
        File.WriteAllText(wolframFileName, wolframData)

        use _ = new AssertionScope()
        writeLine $"noOfEpochs = {noOfEpochs}, noOfDomainPoints = {mp.eeInfModelParams.kernelParams.domainIntervals}"
        writeLine $"startInv = {startInv}, endInv = {endInv}"
        writeLine $"start: food = {startStat.food}, waste = {startStat.waste}, u = {startStat.total}"
        writeLine $"start: ee mean = {startStat.eeStatData.mean}, ee stdDev = {startStat.eeStatData.stdDev}, inf mean = {startStat.infStatData.mean}, inf stdDev = {startStat.infStatData.stdDev}"
        writeLine $"end: food = {endStat.food}, waste = {endStat.waste}, u = {endStat.total}"
        writeLine $"end: ee mean = {endStat.eeStatData.mean}, ee stdDev = {endStat.eeStatData.stdDev}, inf mean = {endStat.infStatData.mean}, inf stdDev = {endStat.infStatData.stdDev}"
        endInv.Should().Be(startInv, nullString) |> ignore

    member private _.getCallerName([<CallerMemberName; Optional; DefaultParameterValue("")>] ?memberName: string) =
        let now = DateTime.Now
        let memberName = defaultArg memberName ""
        $"{memberName}__{now:yyyyMMdd_HHmm}"

    // Flat.
    [<Fact>]
    member t.d100k01e01a0_100K () : unit = runPoissonEvolution mp_d100k01e01a0 100_000 (t.getCallerName())

    // Quadratic in inf space.
    [<Fact>]
    member t.d200k001e01g01_10K () : unit = runPoissonEvolution mp_d200k001e01g01 10_000 (t.getCallerName())

    [<Fact>]
    member t.d200k001e01g01_100K () : unit = runPoissonEvolution mp_d200k001e01g01 100_000 (t.getCallerName())

    // [<Fact>]
    // member t.d200k001e01g01_1M () : unit = runPoissonEvolution mp_d200k001e01g01 1_000_000 (t.getCallerName())

    [<Fact>]
    member t.d100k01e01g01_10K () : unit = runPoissonEvolution mp_d100k01e01g01 10_000 (t.getCallerName())

    [<Fact>]
    member t.d200k01e01g01_10K () : unit = runPoissonEvolution mp_d200k01e01g01 10_000 (t.getCallerName())

    [<Fact>]
    member t.d200k01e01g01_100K () : unit = runPoissonEvolution mp_d200k01e01g01 100_000 (t.getCallerName())

    // [<Fact>]
    // member t.d200k01e01g01_1M () : unit = runPoissonEvolution mp_d200k01e01g01 1_000_000 (t.getCallerName())

    // Quadratic  with small linear factor in inf space.
    [<Fact>]
    member t.d200k001e01g01i01_10K () : unit = runPoissonEvolution mp_d200k001e01g01i01 10_000 (t.getCallerName())

    [<Fact>]
    member t.d200k001e01g01i01_100K () : unit = runPoissonEvolution mp_d200k001e01g01i01 100_000 (t.getCallerName())

    [<Fact>]
    member t.d100k01e01g01i01_10K () : unit = runPoissonEvolution mp_d100k01e01g01i01 10_000 (t.getCallerName())

    [<Fact>]
    member t.d200k01e01g01i01_10K () : unit = runPoissonEvolution mp_d200k01e01g01i01 10_000 (t.getCallerName())

    [<Fact>]
    member t.d200k01e01g01i01_100K () : unit = runPoissonEvolution mp_d200k01e01g01i01 100_000 (t.getCallerName())
