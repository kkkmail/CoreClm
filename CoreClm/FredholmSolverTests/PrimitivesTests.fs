namespace FredholmSolverTests.PrimitivesTests

open System
open System.Diagnostics
open Microsoft.FSharp.NativeInterop
open Xunit
open Xunit.Abstractions
open FluentAssertions
open FredholmSolver.Primitives

type ModelTests(output : ITestOutputHelper) =

    let writeLine s = output.WriteLine s
    let nullString : string = null


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

        diff.Should().BeLessThan(1.0e-10, nullString)
        |> ignore

        // let x2 =
        //     p.xy_x1y1
        //     |> Array.map (fun a -> a |> Array.map domain.integrateValues)
        //     |> XY.create

        // writeLine $"integrate(p.x1y1_xy) = {x1}"
        // writeLine $"integrate(p.xy_x1y1) = {x2}"


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
