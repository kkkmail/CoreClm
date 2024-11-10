namespace ClmTests

open Softellect.DistributedProcessing.Primitives.Common
open Softellect.DistributedProcessing.SolverRunner.OdeSolver

#nowarn "9"

open System
//open GenericOdeSolver.Solver
open Microsoft.FSharp.NativeInterop
open Softellect.OdePackInterop
open Xunit
open Xunit.Abstractions
open FluentAssertions
open DbData.DatabaseTypesDbo
open DbData.DatabaseTypesClm
open DbData.Configuration
open Clm.Model.ModelData
open Clm.CalculationData
//open GenericOdeSolver.Primitives
//open OdeSolver.Solver

/// Use
///     "ContGenAdm.exe add -i 4005000004 -n 9 -m 3 -y 10 -t 250000 -r 1 -g"
/// or
///     "ContGenAdm.exe add -i 4005000004 -n 10 -m 3 -y 10 -t 250000 -r 1 -g"
/// to generate ModelCode.fs. The value "-n 10" produces the file on the boundary
/// of what can be compiled (around 17 - 18 MB) without OutOfMemoryException.
type ModelTests(output : ITestOutputHelper) =

    let writeLine s = output.WriteLine s


    let ModelDataShouldMatchGeneratedCodeImpl (mdUpdate : ModelData -> double[] -> double[]) =
        let eps = 0.000_001
        let cgModelDataParamsWithExtraData = modelDataParamsWithExtraData
        let cgGetTotalSubst = getTotalSubst
        let cgGetTotals = getTotals
        let cgUpdate = update

        let modelDataId = cgModelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.modelDataId
        let mdo = loadModelData getContGenConnectionString modelDataId

        let rnd = Random()
        let x = [| for _ in 1..cgModelDataParamsWithExtraData.regularParams.allSubstData.allSubst.Length -> rnd.NextDouble() |]

        let md =
            match mdo with
            | Ok md ->
                let paramEq = (md.modelData.modelDataParams = cgModelDataParamsWithExtraData.regularParams.modelDataParams)
                writeLine $"(md.modelData.modelDataParams = cgModelDataParamsWithExtraData.modelDataParams) =\n    %A{paramEq}\n"

                let mdModelDataParamsWithExtraData = md.modelData.getModelDataParamsWithExtraData()
                let allParamEq = (mdModelDataParamsWithExtraData.regularParams = cgModelDataParamsWithExtraData.regularParams)
                writeLine $"(mdAllParam.regularParams = cgModelDataParamsWithExtraData.regularParams) =\n    %A{allParamEq}\n"

                let mdGetTotalSubst = md.modelData.modelBinaryData.calculationData.getTotalSubst
                let mdGetTotals = md.modelData.modelBinaryData.calculationData.getTotals

                let cgTotalSubst = cgGetTotalSubst x
                let mdTotalSubst = mdGetTotalSubst x
                writeLine $"diff (must be close to 0.0) = %A{cgTotalSubst - mdTotalSubst}"

                let cgGetTotals = cgGetTotals x
                let mdGetTotals = mdGetTotals x
                let diffTotals =
                    Array.zip cgGetTotals mdGetTotals
                    |> Array.map(fun ((a1, b1), (a2, b2)) -> (a1 - a2) * (a1 - a2) + (b1 - b2) * (b1 - b2))
                    |> Array.sum

                writeLine $"diffTotals (must be close to 0.0) = %A{diffTotals}"

                let cgUpdate = cgUpdate x |> Array.toList
                let mdUpdate = mdUpdate md x |> Array.toList

                let diffUpdate =
                    List.zip cgUpdate mdUpdate
                    |> List.map(fun (a, b) -> (a - b) * (a - b))
                    |> List.sum

                writeLine $"diffUpdate (must be close to 0.0) = %A{diffUpdate}"

                if diffUpdate > eps
                then
                    List.zip cgUpdate mdUpdate
                    |> List.mapi(fun i (a, b) -> i, (a, b, abs (a - b)))
                    |> List.filter (fun (i, (a, b, c)) -> c > eps)
                    |> List.sortByDescending (fun (i, (a, b, c)) -> c, i)
                    |> List.map (fun (i, (a, b, c)) -> writeLine $"i = %A{i}, s = %A{allSubst.[i]}, cg = %A{a}, md = %A{b}, diff = %A{c}")
                    |> ignore

                diffUpdate.Should().BeLessThan(eps, "") |> ignore
                md
            | Error e ->
                writeLine $"Failed to load model data with error: %A{e}."
                failwith "! Error occurred !"

        let mdUpdate = md.modelData.modelBinaryData.calculationData.getDerivative
        let repetitions = [for i in 1..1_000 -> i ]

//        printfn "\n\nCG"
        repetitions |> List.map (fun _ -> cgUpdate x |> ignore) |> ignore
//        printfn "CG completed\n\n"

//        printfn "MD"
        repetitions |> List.map (fun _ -> mdUpdate x |> ignore) |> ignore
//        printfn "MD completed\n\n"
//        printfn "... completed."

        ()

    [<Fact>]
    member _.ModelDataShouldMatchGeneratedCode () : unit =
        let mdUpdate (md : ModelData) = md.modelData.modelBinaryData.calculationData.getDerivative
        ModelDataShouldMatchGeneratedCodeImpl mdUpdate

    //[<Fact>]
    //member _.ModelDataShouldMatchGeneratedCodeForPointerDerivative () : unit =
    //    let mdUpdate (md : ModelData) (x : double[]) : double[] =
    //        let neq = x.Length
    //        let t = 0.0
    //        let callaBack _ _ = ()
    //        let (dx : double[]) = Array.zeroCreate x.Length
    //        use px = fixed &x.[0]
    //        use pdx = fixed &dx.[0]
    //        let calculateDerivative t x = md.modelData.modelBinaryData.calculationData.getDerivative x
    //        let dc : DerivativeCalculator = calculateDerivative |> FullArray

    //        let interop = createUseNonNegativeInterop (callaBack, dc)
    //        do interop.Invoke(ref neq, ref t, px, pdx)
    //        dx

    //    ModelDataShouldMatchGeneratedCodeImpl mdUpdate
