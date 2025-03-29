namespace FredholmSolverTests

open System
open System.Diagnostics
open FluentAssertions.Execution
open Xunit
open Xunit.Abstractions
open FluentAssertions
open FredholmSolver.Primitives
open FredholmSolver.Kernel
open FredholmSolver.Sparse
open Softellect.Analytics.Wolfram
open Softellect.Math.Primitives

type TestUnion =
    | A
    | B of int
    | C of array<int>
    | D of array<(double * int)>


type InnerType =
    {
        a : int
        b : double
    }


type TestType =
    {
        d : int
        c : double
        inner : InnerType
        innerList : list<InnerType>
        innerArray: array<InnerType>
    }


type TestTypeWithArray =
    {
        testUnionArray : array<TestUnion>
        testArray : array<int>
    }


type OuterType =
    {
        testType : TestType
        testUnion : TestUnion
        testTypeWithArray : TestTypeWithArray
    }


type PrimitivesTests (output : ITestOutputHelper) =
    let writeLine s = output.WriteLine s
    let nullString : string = null
    let errTolerance = 1.0e-10

    let domain2D (data : KernelParams) = Domain2D.create (data.domainIntervals.value, data.infMaxValue.value)
    let normalize (data : KernelParams) v = v / (double (data.domainIntervals.value * data.domainIntervals.value))


    let mutationProbability4D_ShouldIntegrateToOneImpl (data : KernelParams) =
        let domain = domain2D data
        let sw = Stopwatch.StartNew()
        let p = MutationProbability4D.create EvolutionType.DifferentialEvolution data.mutationProbabilityData2D
        writeLine $"Took: {sw.Elapsed.TotalSeconds} seconds to create MutationProbability4D."
        sw.Restart()

        let x1 = domain.integrateValues p.x1y1_xy
        let x1Linear = x1.toLinearMatrix()
        let diff = x1Linear.x |> Array.map (fun e -> pown (e - 1.0) 2) |> Array.sum
        writeLine $"Took: {sw.Elapsed.TotalSeconds} seconds to call integrateValues, diff: {diff}."

        use _ = new AssertionScope()
        diff.Should().BeLessThan(errTolerance, nullString) |> ignore

        let x2 = domain.integrateValues p.xy_x1y1
        let total1 = domain.integrateValues x1 |> (normalize data)
        let total2 = domain.integrateValues x2 |> (normalize data)
        total1.Should().BeApproximately(total2, errTolerance, nullString) |> ignore


    /// Creates a "delta" function centered near (0, 0) in the domain,
    /// which is a middle point in ee domain and 0-th point in inf domain.
    let getDeltaU (data : KernelParams) =
        let domain =
            if data.domainIntervals.value % 2 = 0
            then domain2D data
            else failwith "data.noOfIntervals must be odd for this method to work."

        let g i j =
            if (i * 2 = data.domainIntervals.value) && (j = 0)
            then 1.0
            else 0.0

        let v = domain.eeDomain.points.value |> Array.mapi (fun i _ -> domain.infDomain.points.value |> Array.mapi (fun j _ -> g i j)) |> Matrix
        let norm = domain.integrateValues v
        (1.0 / norm) * v

    let data =
        {
            c = 1.0
            d = 2
            inner =
                {
                    a = 3
                    b = 4.0
                }
            innerList =
                [
                    {
                        a = 5
                        b = 6.0
                    }
                    {
                        a = 7
                        b = 8.0
                    }
                ]
            innerArray =
                [|
                    {
                        a = 9
                        b = 10.0
                    }
                    {
                        a = 11
                        b = 12.0
                    }
                |]
        }

    let outputData =
        {
            testType = data
            testUnion = C [| 0; 1; 2; 3 |]
            testTypeWithArray =
                {
                    testUnionArray = [| A; B 1; C [| 1; 2; |]; D [| (3.0, 4); (5.0, 6) |] |]
                    testArray = [| 1; 2; 3; |]
                }
        }


    [<Fact>]
    member _.mutationProbability4D_ShouldIntegrateToOne () : unit =
        mutationProbability4D_ShouldIntegrateToOneImpl KernelParams.defaultValue


    [<Fact>]
    member _.mutationProbability4D_ShouldIntegrateToOneForNarrowData () : unit =
        let kp = KernelParams.defaultValue |> KernelParams.withEps0 Eps0.defaultNarrowValue
        mutationProbability4D_ShouldIntegrateToOneImpl kp


    [<Fact>]
    member _.mutationProbability4D_ShouldIntegrateToOneForWideData () : unit =
        let kp = KernelParams.defaultValue |> KernelParams.withEps0 Eps0.defaultWideValue
        mutationProbability4D_ShouldIntegrateToOneImpl kp


    [<Fact>]
    member _.defaultKernel_ShouldMatchProbability () : unit =
        let data = KernelParams.defaultValue

        let domain = domain2D data
        let sw = Stopwatch.StartNew()
        let p = MutationProbability4D.create EvolutionType.DifferentialEvolution data.mutationProbabilityData2D
        let kernel = KernelData.create EvolutionType.DifferentialEvolution data
        writeLine $"{sw.Elapsed.TotalSeconds}."

        let x1 = domain.integrateValues kernel.kernel.value
        writeLine $"{sw.Elapsed.TotalSeconds}."

        use _ = new AssertionScope()
        let x2 = domain.integrateValues p.xy_x1y1

        let total =
            (x1.value |> Array.concat)
            |> Array.zip (x2.value |> Array.concat)
            |> Array.map (fun (a, b) -> pown (a - b) 2)
            |> Array.sum
            |> (normalize data)

        total.Should().BeApproximately(0.0, errTolerance, nullString) |> ignore


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

        let b = a.toSparseValueArray().value |> Array.map (fun e -> e.value4D)
        writeLine $"{a}"
        b.Should().BeEquivalentTo([| 0; 1; 10; 11; 100; 101; 110; 111; 1000; 1001; 1010; 1011; 1100; 1101; 1110; 1111 |], nullString) |> ignore


    [<Fact>]
    member _.delta_ShouldIntegrateToOne () : unit =
        let data = KernelParams.defaultValue

        let u = (getDeltaU data).toLinearMatrix()
        let domain = domain2D data
        let norm = domain.integrateValues u
        let norm2 = domain.norm u

        use _ = new AssertionScope()
        norm. Should().BeApproximately(1.0, errTolerance, nullString) |> ignore
        norm2. Should().BeApproximately(1.0, errTolerance, nullString) |> ignore


    [<Fact>]
    member _.mean_ShouldBeConsistent () : unit =
        let data = KernelParams.defaultValue

        let u = (getDeltaU data).toLinearMatrix()
        let domain = domain2D data
        let mx, my = domain.mean u
        writeLine $"mx = {mx}, my = {my}."

        use _ = new AssertionScope()
        mx.Should().BeApproximately(0.0, errTolerance, nullString) |> ignore

        // my gets some weird value of 0.12376237623762376, which could be correct as we don't (yet) have points at the boundary.
        my.Should().BeApproximately(0.0, errTolerance, nullString) |> ignore


    [<Fact>]
    member _.stdDev_ShouldBeConsistent () : unit =
        let data = KernelParams.defaultValue

        let u = (getDeltaU data).toLinearMatrix()
        let domain = domain2D data
        let sx, sy = domain.stdDev u

        use _ = new AssertionScope()
        sx.Should().BeApproximately(0.0, errTolerance, nullString) |> ignore
        sy.Should().BeApproximately(0.0, errTolerance, nullString) |> ignore


    [<Fact>]
    member _.integrate2D_ShouldMatch () : unit =
        let rnd = Random(1)
        let n = 100
        let domain = Domain2D.create (n, 25.0)

        let m =
            domain.eeDomain.points.value
            |> Array.map (fun _ -> domain.infDomain.points.value |> Array.map (fun _ -> rnd.NextDouble()))
            |> Matrix

        let s =
            m.value
            |> Array.mapi (fun i e -> e |> Array.mapi (fun j v -> { i = i; j = j; value2D = v }))
            |> Array.concat
            |> SparseArray2D<double>.SparseArray2D

        let mv = domain.integrateValues m
        let ms = domain.integrateValues s
        writeLine $"mv = {mv}, ms = {ms}."
        mv.Should().BeApproximately(ms, 0.00001, nullString) |> ignore


    [<Fact>]
    member _.toWolframNotation_ShouldWorkForSmallFloats () : unit =
        let x = 1.0e-20

        let output = toWolframNotation x
        output.Should().Be("1.000000*^-020", nullString) |> ignore


    [<Fact>]
    member _.toWolframNotation_ShouldWorkForRecords () : unit =
        let output = toWolframNotation data
        output.Should().Be("{ 1.000000*^+000, 2, { 3, 4.000000*^+000 }, { { 9, 1.000000*^+001 }, { 11, 1.200000*^+001 } }, { { 5, 6.000000*^+000 }, { 7, 8.000000*^+000 } } }", nullString) |> ignore


    [<Fact>]
    member _.toWolframNotation_ShouldWorkForListOfLists () : unit =
        let x =
            [
                [ 1; 2 ; 3 ]
                [ 4; 5; 6 ]
            ]

        let s = @"{ { 1, 2, 3 },
{ 4, 5, 6 } }"

        let output = toWolframNotation x
        output.Should().Be(s, nullString) |> ignore


    [<Fact>]
    member _.toOutputString_ShouldWork () : unit =
        let output = toOutputString outputData
        let s = @"{
  testType =
    {
      d = 2
      c = 1
      inner =
        {
          a = 3
          b = 4
        }
      innerList =
        [
          {
            a = 5
            b = 6
          }
          {
            a = 7
            b = 8
          }
        ]
      innerArray =
        [|
          {
            a = 9
            b = 10
          }
          {
            a = 11
            b = 12
          }
        |]
    }
  testUnion = C [| 0; 1; 2; 3 |]
  testTypeWithArray =
    {
      testUnionArray =
        [|
          A
          B 1
          C [| 1; 2 |]
          D [| (3, 4); (5, 6) |]
        |]
      testArray = [| 1; 2; 3 |]
    }
}"

        output.Should().Be(s, nullString) |> ignore
