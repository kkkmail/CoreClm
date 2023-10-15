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

    [<Fact>]
    member t.poissonEvolution_ShouldKeepInvariant_10K () : unit =
        let noOfEpochs = 10_000
        let noOfDomainPoints = 100
        let md = EeInfIntModelParams.defaultNonLinearValue |> EeInfIntModelParams.withDomainIntervals (DomainIntervals noOfDomainPoints) |> EeInfIntModelParams.withK0 K0.defaultSmallValue
        let model = EeInfIntModel.create md
        let initialValue = model.intInitialValues
        let startInv = model.invariant initialValue
        let startStat = calculateIntStat model initialValue
        let ps = Random 1 |> PoissonSampler.create
        let result = [|for _ in 1..noOfEpochs -> () |] |> Array.fold (fun acc _ -> model.evolve ps acc) initialValue
        let endInv = model.invariant result
        let endStat = calculateIntStat model result

        use _ = new AssertionScope()
        writeLine $"noOfEpochs = {noOfEpochs}, noOfDomainPoints = {noOfDomainPoints}"
        writeLine $"startInv = {startInv}, endInv = {endInv}"
        writeLine $"start: food = {startStat.food}, waste = {startStat.waste}, u = {startStat.total}"
        writeLine $"start: ee mean = {startStat.eeStatData.mean}, ee stdDev = {startStat.eeStatData.stdDev}, inf mean = {startStat.infStatData.mean}, inf stdDev = {startStat.infStatData.stdDev}"
        writeLine $"end: food = {endStat.food}, waste = {endStat.waste}, u = {endStat.total}"
        writeLine $"end: ee mean = {endStat.eeStatData.mean}, ee stdDev = {endStat.eeStatData.stdDev}, inf mean = {endStat.infStatData.mean}, inf stdDev = {endStat.infStatData.stdDev}"
        endInv.Should().Be(startInv, nullString) |> ignore


    [<Fact>]
    member t.poissonEvolution_ShouldKeepInvariant_100K () : unit =
        let noOfEpochs = 100_000
        let noOfDomainPoints = 100
        let md = EeInfIntModelParams.defaultNonLinearValue |> EeInfIntModelParams.withDomainIntervals (DomainIntervals noOfDomainPoints) |> EeInfIntModelParams.withK0 K0.defaultSmallValue
        let model = EeInfIntModel.create md
        let initialValue = model.intInitialValues
        let startInv = model.invariant initialValue
        let startStat = calculateIntStat model initialValue
        let ps = Random 1 |> PoissonSampler.create
        let result = [|for _ in 1..noOfEpochs -> () |] |> Array.fold (fun acc _ -> model.evolve ps acc) initialValue
        let endInv = model.invariant result
        let endStat = calculateIntStat model result

        use _ = new AssertionScope()
        writeLine $"noOfEpochs = {noOfEpochs}, noOfDomainPoints = {noOfDomainPoints}"
        writeLine $"startInv = {startInv}, endInv = {endInv}"
        writeLine $"start: food = {startStat.food}, waste = {startStat.waste}, u = {startStat.total}"
        writeLine $"start: ee mean = {startStat.eeStatData.mean}, ee stdDev = {startStat.eeStatData.stdDev}, inf mean = {startStat.infStatData.mean}, inf stdDev = {startStat.infStatData.stdDev}"
        writeLine $"end: food = {endStat.food}, waste = {endStat.waste}, u = {endStat.total}"
        writeLine $"end: ee mean = {endStat.eeStatData.mean}, ee stdDev = {endStat.eeStatData.stdDev}, inf mean = {endStat.infStatData.mean}, inf stdDev = {endStat.infStatData.stdDev}"
        endInv.Should().Be(startInv, nullString) |> ignore


    [<Fact>]
    member t.poissonEvolution_ShouldKeepInvariant_1M () : unit =
        let noOfEpochs = 1_000_000
        let noOfDomainPoints = 100
        let md = EeInfIntModelParams.defaultNonLinearValue |> EeInfIntModelParams.withDomainIntervals (DomainIntervals noOfDomainPoints) |> EeInfIntModelParams.withK0 K0.defaultSmallValue
        let model = EeInfIntModel.create md
        let initialValue = model.intInitialValues
        let startInv = model.invariant initialValue
        let startStat = calculateIntStat model initialValue
        let ps = Random 1 |> PoissonSampler.create
        let result = [|for _ in 1..noOfEpochs -> () |] |> Array.fold (fun acc _ -> model.evolve ps acc) initialValue
        let endInv = model.invariant result
        let endStat = calculateIntStat model result

        use _ = new AssertionScope()
        writeLine $"noOfEpochs = {noOfEpochs}, noOfDomainPoints = {noOfDomainPoints}"
        writeLine $"startInv = {startInv}, endInv = {endInv}"
        writeLine $"start: food = {startStat.food}, waste = {startStat.waste}, u = {startStat.total}"
        writeLine $"start: ee mean = {startStat.eeStatData.mean}, ee stdDev = {startStat.eeStatData.stdDev}, inf mean = {startStat.infStatData.mean}, inf stdDev = {startStat.infStatData.stdDev}"
        writeLine $"end: food = {endStat.food}, waste = {endStat.waste}, u = {endStat.total}"
        writeLine $"end: ee mean = {endStat.eeStatData.mean}, ee stdDev = {endStat.eeStatData.stdDev}, inf mean = {endStat.infStatData.mean}, inf stdDev = {endStat.infStatData.stdDev}"
        endInv.Should().Be(startInv, nullString) |> ignore
