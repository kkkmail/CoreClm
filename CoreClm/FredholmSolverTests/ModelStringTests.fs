namespace FredholmSolverTests

open FredholmSolver.EeInfIntModel
open Xunit
open FluentAssertions
open FredholmSolverTests.PoissonTestData

type ModelStringTestData() =
    static member Data =
        let theoryData = TheoryData<EeInfIntModelParams, int, string>()

        for x, y, z in
            [|
                EeInfIntModelParams.defaultValue, 100_000, "d100kI1_0gS01"
                mp_d100k01e01a0, 100_000, "___"
                mp_d200k1e01g01, 100_000, "___"
                // mp_d200k1e01g01f1T, 100_000, "___"
                // mp_d200k1e01g01f1P, 100_000, "___"
                // mp_d200k1e01g01f1E, 100_000, "___"
                // mp_d200k1e01g01a001, 100_000, "___"
                // mp_d200k1e01g01a0001, 100_000, "___"
                // mp_d200k1e01g01a0001f1P, 100_000, "___"
                // mp_d200k1e01g01i01, 100_000, "___"
                // mp_d200k10e01g01i1, 100_000, "___"
                // mp_d200k10e01g01i1f1P, 100_000, "___"
            |] do theoryData.Add(x, y, z)

        theoryData


module ModelStringTests =
    let nullString : string = null


    [<Theory>]
    [<MemberData(nameof(ModelStringTestData.Data), MemberType = typeof<ModelStringTestData>)>]
    let modelString_ShouldWork(mp : EeInfIntModelParams, noOfEpochs : int, expectedName : string) : unit =
        let ms = mp.modelString
        ms.Should().Be(expectedName, nullString) |> ignore
