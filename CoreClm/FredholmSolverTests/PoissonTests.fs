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
    member t.poissonEvolution_ShouldKeepInvariant () : unit =
        let md = EeInfIntModelParams.defaultValue |> EeInfIntModelParams.withDomainIntervals (DomainIntervals 10) |> EeInfIntModelParams.withK0 K0.defaultVerySmallValue
        let model = EeInfIntModel.create md
        let initialValue = model.intInitialValues
        let startInv = model.invariant initialValue
        let ps = Random 1 |> PoissonSampler.create
        let result = [|for _ in 1..10 -> () |] |> Array.fold (fun acc _ -> model.evolve ps acc) initialValue
        let endInv = model.invariant result
        writeLine $"startInv = {startInv}, endInv = {endInv}"
        endInv.Should().Be(startInv, nullString) |> ignore
