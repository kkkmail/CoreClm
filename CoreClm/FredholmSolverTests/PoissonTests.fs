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

    let runPoissonEvolution noOfEpochs noOfDomainPoints k0 name =
        let md =
            EeInfIntModelParams.defaultNonLinearValue.named name
            |> EeInfIntModelParams.withDomainIntervals (DomainIntervals noOfDomainPoints)
            |> EeInfIntModelParams.withK0 k0

        let model = EeInfIntModel.create md
        let initialValue = model.intInitialValues
        let startInv = model.invariant initialValue
        let startStat = calculateIntStat model initialValue
        let ps = Random 1 |> PoissonSampler.create

        let chartInitData =
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

        let chartDataUpdater = AsyncChartIntDataUpdater(ChartIntDataUpdater(), chartInitData)

        let getChartSliceData e i : ChartSliceIntData =
            {
                epochNumber = i
                progress = 0.0m
                statData  = calculateIntStat model e
                substanceData  = None
            }

        let outputChart (chartData : ChartIntData) =
            writeLine "epochNumber,food,waste,total,invariant,eeMean,eeStdDev,infMean,infStdDev"

            chartData.allChartData
            |> List.rev
            |> List.map (fun e -> writeLine $"{e.epochNumber},{e.statData.food},{e.statData.waste},{e.statData.total},{e.statData.invariant},{e.statData.eeStatData.mean},{e.statData.eeStatData.stdDev},{e.statData.infStatData.mean},{e.statData.infStatData.stdDev}")
            |> ignore

        let evolve e i =
            let e1 = model.evolve ps e
            getChartSliceData e1 i |> chartDataUpdater.addContent
            e1

        let result = [|for i in 1..noOfEpochs -> i |] |> Array.fold evolve initialValue
        let endInv = model.invariant result
        let endStat = calculateIntStat model result
        let chartData = chartDataUpdater.getContent()
        chartData |> outputChart

        use _ = new AssertionScope()
        writeLine $"noOfEpochs = {noOfEpochs}, noOfDomainPoints = {noOfDomainPoints}"
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

    [<Fact>]
    member t.mp_d200k001e01g01_10K () : unit = runPoissonEvolution 10_000 200 K0.defaultVerySmallValue (t.getCallerName())

    [<Fact>]
    member t.mp_d200k001e01g01_100K () : unit = runPoissonEvolution 100_000 200 K0.defaultVerySmallValue (t.getCallerName())

    [<Fact>]
    member t.mp_d200k001e01g01_1M () : unit = runPoissonEvolution 1_000_000 200 K0.defaultVerySmallValue (t.getCallerName())

    [<Fact>]
    member t.mp_d200k01e01g01_10K () : unit = runPoissonEvolution 10_000 200 K0.defaultSmallValue (t.getCallerName())

    [<Fact>]
    member t.mp_d200k01e01g01_100K () : unit = runPoissonEvolution 100_000 200 K0.defaultSmallValue (t.getCallerName())

    [<Fact>]
    member t.mp_d200k01e01g01_1M () : unit = runPoissonEvolution 1_000_000 200 K0.defaultSmallValue (t.getCallerName())
