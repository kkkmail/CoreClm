namespace FredholmSolverTests.PrimitivesTests

open System
open System.Diagnostics
open FluentAssertions.Execution
open Microsoft.FSharp.NativeInterop
open Xunit
open Xunit.Abstractions
open FluentAssertions
open FredholmSolver.Primitives
open FredholmSolver.Kernel

type ModelTests(output : ITestOutputHelper) =

    let writeLine s = output.WriteLine s
    let nullString : string = null
    let errTolerance = 1.0e-10


    [<Fact>]
    member _.mutationProbability4D_ShouldIntegrateToOne () : unit =
        let sw = Stopwatch.StartNew()

        let data =
            {
                noOfIntervals = 100
                l2 = 25
                epsEe = fun _ -> 0.02
                epsInf = fun _ -> 0.02
            }

        let domain = Domain2D.create data.noOfIntervals data.l2

        let m2Data =
            {
                eeMutationProbabilityData =
                    {
                        domain = domain.eeDomain
                        zeroThreshold = MutationProbabilityData.defaultZeroThreshold
                        epsFunc = data.epsEe
                    }
                infMutationProbabilityData =
                    {
                        domain = domain.infDomain
                        zeroThreshold = MutationProbabilityData.defaultZeroThreshold
                        epsFunc = data.epsInf
                    }
            }

        // writeLine $"domain.eeDomain: {domain.eeDomain}"
        // writeLine $"domain.infDomain: {domain.infDomain}"

        let p = MutationProbability4D.create m2Data

        writeLine $"{sw.Elapsed.TotalSeconds}."

        let x1 =
            p.x1y1_xy
            |> Array.map (fun a -> a |> Array.map domain.integrateValues)
            |> Matrix

        let x1Linear = x1.toLinearMatrix()

        let diff =
            x1Linear.x
            |> Array.map (fun e -> pown (e - 1.0) 2)
            |> Array.sum

        writeLine $"{sw.Elapsed.TotalSeconds}."

        use _ = new AssertionScope()
        diff.Should().BeLessThan(errTolerance, nullString) |> ignore

        let x2 =
            p.xy_x1y1
            |> Array.map (fun a -> a |> Array.map domain.integrateValues)
            |> Matrix

        let total1 = (x1.value |> Array.concat |> Array.sum) / (double (data.noOfIntervals * data.noOfIntervals))
        let total2 = (x2.value |> Array.concat |> Array.sum) / (double (data.noOfIntervals * data.noOfIntervals))
        total1.Should().BeApproximately(total2, errTolerance, nullString) |> ignore


    [<Fact>]
    member _.toLinearMatrix_ShouldWork () : unit =
        let m = [| [| 1; 2 |]; [| 3; 4 |] |] |> Matrix
        let lm = m.toLinearMatrix().x
        lm.Should().BeEquivalentTo([| 1; 2; 3; 4 |], nullString) |> ignore


    [<Fact>]
    member _.toMatrix_ShouldWork () : unit =
        let lm = [| [| 1; 2 |]; [| 3; 4 |] |] |> LinearMatrix.create
        let m = lm.toMatrix().value
        m.Should().BeEquivalentTo([| [| 1; 2 |]; [| 3; 4 |] |], nullString) |> ignore


    [<Fact>]
    member _.createSparseArray4D_ShouldWork () : unit =
        let a =
            [|
                for i in 0..1 ->
                    [|
                        for j in 0..1 ->
                            [| for k in 0..1 -> [| for l in 0..1 -> 1000 * i + 100 * j + 10 * k + l |] |]
                            |> SparseArray2D<int>.create 0
                    |]
            |]
            |> SparseArray4D

        let b = a.toSparseArray() |> Array.map (fun e -> e.value4D)
        writeLine $"{a}"
        b.Should().BeEquivalentTo([| 0; 1; 10; 11; 100; 101; 110; 111; 1000; 1001; 1010; 1011; 1100; 1101; 1110; 1111 |], nullString) |> ignore
