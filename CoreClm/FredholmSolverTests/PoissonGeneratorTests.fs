namespace FredholmSolverTests

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System
open FredholmSolver.Solver
open Xunit
open Xunit.Abstractions
open Softellect.DistributedProcessing.Proxy.ModelGenerator
open Softellect.Math.Models
open PoissonTestData

type PoissonGeneratorTests(output : ITestOutputHelper) =

    let writeLine s = output.WriteLine s
    let systemProxy = ModelGeneratorSystemProxy.create()
    let ne_100K = NoOfEpochs 100_000
    let ne_200K = NoOfEpochs 200_000
    let ne_500K = NoOfEpochs 500_000
    let ne_1M = NoOfEpochs 1_000_000

    let failIfError result =
        writeLine $"result: '%A{result}'."

        match result with
        | Ok _ -> ()
        | Error e -> failwith $"Error: '{e}'."


    member private _.getCallerName([<CallerMemberName; Optional; DefaultParameterValue("")>] ?memberName: string) =
        let now = DateTime.Now
        let memberName = defaultArg memberName ""
        $"{memberName}__{now:yyyyMMdd_HHmm}"

    // ===================================================
    // ===================================================
    // d100, 100 K - quick test.

    // [<Fact>]
    // member t.d100k10e01g01i1_100K_V1() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d100k10e01g01i1 ne_100K (t.getCallerName())
    //     FredholmSolver.PoissonSolver.poissonModelGenerator systemProxy p |> failIfError


    [<Fact>]
    member t.d100k10e01g01i1_100K_V2() : unit =
        let p = PoissonInitialData.defaultValue mp_d100k10e01g01i1 ne_100K (t.getCallerName())
        FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p |> failIfError

    // ===================================================
    // ===================================================
    // d200, 100 K - quick test.

    // [<Fact>]
    // member t.d200k10e01g01i1_100K_V1() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d200k10e01g01i1 ne_100K (t.getCallerName())
    //     FredholmSolver.PoissonSolver.poissonModelGenerator systemProxy p |> failIfError


    [<Fact>]
    member t.d200k10e01g01i1_100K_V2() : unit =
        let p = PoissonInitialData.defaultValue mp_d200k10e01g01i1 ne_100K (t.getCallerName())
        FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p |> failIfError

    // // ===================================================
    // // ===================================================
    // // d500, 100 K - as in the article.
    //
    // [<Fact(Skip = "Too large and slow.")>]
    // member t.d500k1e005g01a002f1E_100K_V1() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d500k1e005g01a002f1E ne_100K (t.getCallerName())
    //     FredholmSolver.PoissonSolver.poissonModelGenerator systemProxy p |> failIfError
    //
    //
    // [<Fact>]
    // member t.d500k1e005g01a002f1E_100K_V2() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d500k1e005g01a002f1E ne_100K (t.getCallerName())
    //     FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p |> failIfError
    //
    // // --------------------------------------------------
    //
    // [<Fact(Skip = "Too large and slow.")>]
    // member t.d500k1e01g01a002f1E_100K_V1() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d500k1e01g01a002f1E ne_100K (t.getCallerName())
    //     FredholmSolver.PoissonSolver.poissonModelGenerator systemProxy p |> failIfError
    //
    //
    // [<Fact>]
    // member t.d500k1e01g01a002f1E_100K_V2() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d500k1e01g01a002f1E ne_100K (t.getCallerName())
    //     FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p |> failIfError
    //
    // // --------------------------------------------------
    //
    // [<Fact(Skip = "Too large and slow.")>]
    // member t.d500k1e005g01a002i10f1E_100K_V1() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d500k1e005g01a002i10f1E ne_100K (t.getCallerName())
    //     FredholmSolver.PoissonSolver.poissonModelGenerator systemProxy p |> failIfError
    //
    //
    // [<Fact>]
    // member t.d500k1e005g01a002i10f1E_100K_V2() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d500k1e005g01a002i10f1E ne_100K (t.getCallerName())
    //     FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p |> failIfError
    //
    // // --------------------------------------------------
    //
    // [<Fact(Skip = "Too large and slow.")>]
    // member t.d500k1e01g01a002i10f1E_100K_V1() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d500k1e01g01a002i10f1E ne_100K (t.getCallerName())
    //     FredholmSolver.PoissonSolver.poissonModelGenerator systemProxy p |> failIfError
    //
    //
    // [<Fact>]
    // member t.d500k1e01g01a002i10f1E_100K_V2() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d500k1e01g01a002i10f1E ne_100K (t.getCallerName())
    //     FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p |> failIfError
    //
    // // ===================================================
    // // ===================================================
    // // d500, 200 K - as in the article.
    //
    // [<Fact>]
    // member t.d500k1e005g01a002f1E_200K_V2() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d500k1e005g01a002f1E ne_200K (t.getCallerName())
    //     FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p |> failIfError
    //
    //
    // [<Fact>]
    // member t.d500k1e01g01a002f1E_200K_V2() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d500k1e01g01a002f1E ne_200K (t.getCallerName())
    //     FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p |> failIfError
    //
    //
    // [<Fact>]
    // member t.d500k1e005g01a002i10f1E_200K_V2() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d500k1e005g01a002i10f1E ne_200K (t.getCallerName())
    //     FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p |> failIfError
    //
    //
    // [<Fact>]
    // member t.d500k1e01g01a002i10f1E_200K_V2() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d500k1e01g01a002i10f1E ne_200K (t.getCallerName())
    //     FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p |> failIfError
    //
    // // ===================================================
    // // ===================================================
    // // d500, 500 K - as in the article.
    //
    // [<Fact>]
    // member t.d500k1e005g01a002f1E_500K_V2() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d500k1e005g01a002f1E ne_500K (t.getCallerName())
    //     FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p |> failIfError
    //
    //
    // [<Fact>]
    // member t.d500k1e01g01a002f1E_500K_V2() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d500k1e01g01a002f1E ne_500K (t.getCallerName())
    //     FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p |> failIfError
    //
    //
    // [<Fact>]
    // member t.d500k1e005g01a002i10f1E_500K_V2() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d500k1e005g01a002i10f1E ne_500K (t.getCallerName())
    //     FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p |> failIfError
    //
    //
    // [<Fact>]
    // member t.d500k1e01g01a002i10f1E_500K_V2() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d500k1e01g01a002i10f1E ne_500K (t.getCallerName())
    //     FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p |> failIfError
    //
    // // ===================================================
    // // ===================================================
    // // d1000, 1M - x2 D as in the article.
    //
    // [<Fact>]
    // member t.d1000k1e005g01a002f1E_1M_V2() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d1000k1e005g01a002f1E ne_1M (t.getCallerName())
    //     FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p |> failIfError
    //
    //
    // [<Fact>]
    // member t.d1000k1e01g01a002f1E_1M_V2() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d1000k1e01g01a002f1E ne_1M (t.getCallerName())
    //     FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p |> failIfError
    //
    //
    // [<Fact>]
    // member t.d1000k1e005g01a002i10f1E_1M_V2() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d1000k1e005g01a002i10f1E ne_1M (t.getCallerName())
    //     FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p |> failIfError
    //
    //
    // [<Fact>]
    // member t.d1000k1e01g01a002i10f1E_1M_V2() : unit =
    //     let p = PoissonInitialData.defaultValue mp_d1000k1e01g01a002i10f1E ne_1M (t.getCallerName())
    //     FredholmSolver.PoissonSolver2.poissonModelGenerator systemProxy p |> failIfError
    //
    // // ===================================================
    // // ===================================================
