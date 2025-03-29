namespace FredholmSolverTests

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System
open FredholmSolver.Solver
open Xunit
open Xunit.Abstractions
open Softellect.DistributedProcessing.Proxy.ModelGenerator
open Softellect.Math.Models


type PoissonGeneratorTests(output : ITestOutputHelper) =
    let writeLine s = output.WriteLine s

    let failIfError result =
        writeLine $"result: '%A{result}'."

        match result with
        | Ok _ -> ()
        | Error e -> failwith $"Error: '{e}'."


    member private _.getCallerName([<CallerMemberName; Optional; DefaultParameterValue("")>] ?memberName: string) =
        let now = DateTime.Now
        let memberName = defaultArg memberName ""
        $"{memberName}__{now:yyyyMMdd_HHmm}"


    [<Fact>]
    member t.generateModel_shouldWork() : unit =
        let name = t.getCallerName()
        let mp = PoissonTestData.mp_d100k10e01g01i1
        let noOfEpochs = NoOfEpochs 100_000
        let p = PoissonParam.defaultValue mp noOfEpochs name
        let systemProxy = ModelGeneratorSystemProxy.create()
        FredholmSolver.PoissonSolver.poissonModelGenerator systemProxy p.initialData |> failIfError


    [<Fact>]
    member t.generateModel2_shouldWork() : unit =
        let name = t.getCallerName()
        let mp = PoissonTestData.mp_d100k10e01g01i1
        let noOfEpochs = NoOfEpochs 100_000
        let p = PoissonParam.defaultValue mp noOfEpochs name
        let systemProxy = ModelGeneratorSystemProxy.create()
        FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p.initialData |> failIfError
