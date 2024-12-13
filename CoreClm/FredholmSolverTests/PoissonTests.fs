namespace FredholmSolverTests

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System
open FluentAssertions
open FredholmSolver.Primitives
open FredholmSolver.Kernel
open FredholmSolver
open FredholmSolver.EeInfIntModel
open FredholmSolver.PoissonSolver
open Xunit
open Xunit.Abstractions
open FredholmSolverTests.PoissonTestData
open Softellect.DistributedProcessing.Proxy.ModelGenerator

/// Naming conventions (all must be specified unless noted otherwise).
/// All numbers smaller than 1 are prefixed with a relevant letter, e.g. k0 = 0.01 becomes k01.
/// Current exceptions are domain points, which is just an integer, e.g. for 200 points use d200 and
/// inf space linear factor. If it larger than one then add a zero at the end, e.g. i = 1.0 becomes i10.
///     1. d - domain points, e.g. d200.
///     2. k - k0, e.g. k01.
///     3. e - eps in mutation probability, e.g. e01.
///     4. g - gamma0, default is 0.01.
///     5. a - asymmetry factor, default is 0.01. Can be skipped in the name if default is used.
///     6. i - inf space linear factor, default is 0.0. Can be skipped in the name if default is used.
///     7. w - waste rate, default is 0.01. Can be skipped if default is used.
///
///     8. f - total number of all (mostly food) molecules, e.g. f1T for 10^12 molecules. Default is 10^9. Can be skipped in the name if default is used.
///     9. u - total number of initial u molecules.
///    10. s - shift in the initial delta distribution of u, default is (0, 0). Can be skipped in the name if default is used.
///    11. r - random seed value, default is 1. Can be skipped in the name if default is used.
///    12. t - number of steps in time.
type PoissonTests (output : ITestOutputHelper) =
    let writeLine s = output.WriteLine s
    let nullString : string = null

    let createModel (mp : EeInfIntModelParams) name =
        let md = mp.named name
        let model = EeInfIntModel.create md
        model


    let runPoissonEvolution mp noOfEpochs name =
        let p = PoissonParam.defaultValue mp (NoOfEpochs noOfEpochs) name
        let startInv, endInv = runPoissonEvolution writeLine p
        endInv.Should().Be(startInv, nullString) |> ignore

    // ===================================================================================

    member private _.getCallerName([<CallerMemberName; Optional; DefaultParameterValue("")>] ?memberName: string) =
        let now = DateTime.Now
        let memberName = defaultArg memberName ""
        $"{memberName}__{now:yyyyMMdd_HHmm}"

    // ===================================================================================

    [<Fact>]
    member _.mutationProbability2D_ShouldCreate () : unit =
        let model = createModel mp_d100k1e01g01 "No name"
        let p = model.intModelParams.eeInfModelParams.kernelParams
        // let domain2D = Domain2D.create p.domainIntervals.value p.infMaxValue.value

        // From KernelData.create.
        let mp2 =
            {
                eeMutationProbabilityParams =
                    {
                        domainParams = p.eeDomainParams
                        zeroThreshold = p.zeroThreshold
                        epsFuncValue = p.epsEeFuncValue
                    }

                infMutationProbabilityParams =
                    {
                        domainParams = p.infDomainParams
                        zeroThreshold = p.zeroThreshold
                        epsFuncValue = p.epsInfFuncValue
                    }
            }

        let mp4 = MutationProbability4D.create EvolutionType.DiscreteEvolution mp2

        mp4.Should().NotBeNull(nullString) |> ignore

    // ===================================================================================

    // Flat.
    [<Fact>]
    member t.d100k01e01a0_50K () : unit = runPoissonEvolution mp_d100k01e01a0 50_000 (t.getCallerName())

    [<Fact>]
    member t.d100k01e01a0_100K () : unit = runPoissonEvolution mp_d100k01e01a0 100_000 (t.getCallerName())

    [<Fact>]
    member t.d100k01e01a0_100K_inf2 () : unit =
        let mp = mp_d100k01e01a0.withInfMaxValue (InfMaxValue 2.0)
        runPoissonEvolution mp 100_000 (t.getCallerName())

    [<Fact>]
    member t.d100k01e01a0_100K_inf2_deltaMiddle () : unit =
        let mp = mp_d100k01e01a0.withInfMaxValue (InfMaxValue 2.0)
        let mp1 = mp.withProtocellInitParams (EeInfDiffModel.ProtocellInitParams.DeltaEeInfShifted (0, 0))
        runPoissonEvolution mp1 100_000 (t.getCallerName())

    // ===================================================================================

    // // Flat.
    // [<Fact>]
    // member t.d100k1e01a0_100K () : unit = runPoissonEvolution mp_d100k1e01a0 100_000 (t.getCallerName())
    //
    // [<Fact>]
    // member t.d100k1e01a0_100K_inf2 () : unit =
    //     let mp = mp_d100k1e01a0.withInfMaxValue (InfMaxValue 2.0)
    //     runPoissonEvolution mp 100_000 (t.getCallerName())
    //
    // [<Fact>]
    // member t.d100k1e01a0_100K_inf2_deltaMiddle () : unit =
    //     let mp = mp_d100k1e01a0.withInfMaxValue (InfMaxValue 2.0)
    //     let mp1 = mp.withProtocellInitParams (EeInfDiffModel.ProtocellInitParams.DeltaEeInfShifted (0, 0))
    //     runPoissonEvolution mp1 100_000 (t.getCallerName())

    // ===================================================================================

    // Flat.
    [<Fact>]
    member t.d100k1e01a0_100K () : unit = runPoissonEvolution mp_d100k1e01a0 100_000 (t.getCallerName())

    [<Fact>]
    member t.d100k1e01a0_100K_inf2 () : unit =
        let mp = mp_d100k1e01a0.withInfMaxValue (InfMaxValue 2.0)
        runPoissonEvolution mp 100_000 (t.getCallerName())

    [<Fact>]
    member t.d100k1e01a0_100K_inf2_deltaMiddle () : unit =
        let mp = mp_d100k1e01a0.withInfMaxValue (InfMaxValue 2.0)
        let mp1 = mp.withProtocellInitParams (EeInfDiffModel.ProtocellInitParams.DeltaEeInfShifted (0, 0))
        runPoissonEvolution mp1 100_000 (t.getCallerName())

    // ===================================================================================

    // Quadratic in inf space.
    [<Fact>]
    member t.d200k1e01g01_10K () : unit = runPoissonEvolution mp_d200k1e01g01 10_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01_50K () : unit = runPoissonEvolution mp_d200k1e01g01 50_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01_100K () : unit = runPoissonEvolution mp_d200k1e01g01 100_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01f1T_50K () : unit = runPoissonEvolution mp_d200k1e01g01f1T 50_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01f1T_100K () : unit = runPoissonEvolution mp_d200k1e01g01f1T 100_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01f1P_50K () : unit = runPoissonEvolution mp_d200k1e01g01f1P 50_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01f1P_100K () : unit = runPoissonEvolution mp_d200k1e01g01f1P 100_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01f1E_50K () : unit = runPoissonEvolution mp_d200k1e01g01f1E 50_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01f1E_100K () : unit = runPoissonEvolution mp_d200k1e01g01f1E 100_000 (t.getCallerName())

    // a = 0.001

    [<Fact>]
    member t.d200k1e01g01a001_50K () : unit = runPoissonEvolution mp_d200k1e01g01a001 50_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01a001_100K () : unit = runPoissonEvolution mp_d200k1e01g01a001 100_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01a001f1T_50K () : unit = runPoissonEvolution mp_d200k1e01g01a001f1T 50_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01a001f1T_100K () : unit = runPoissonEvolution mp_d200k1e01g01a001f1T 100_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01a001f1P_50K () : unit = runPoissonEvolution mp_d200k1e01g01a001f1P 50_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01a001f1P_100K () : unit = runPoissonEvolution mp_d200k1e01g01a001f1P 100_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01a001f1E_50K () : unit = runPoissonEvolution mp_d200k1e01g01a001f1E 50_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01a001f1E_100K () : unit = runPoissonEvolution mp_d200k1e01g01a001f1E 100_000 (t.getCallerName())

    // a = 0.0001

    [<Fact>]
    member t.d200k1e01g01a0001_50K () : unit = runPoissonEvolution mp_d200k1e01g01a0001 50_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01a0001_100K () : unit = runPoissonEvolution mp_d200k1e01g01a0001 100_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01a0001f1T_50K () : unit = runPoissonEvolution mp_d200k1e01g01a0001f1T 50_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01a0001f1T_100K () : unit = runPoissonEvolution mp_d200k1e01g01a0001f1T 100_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01a0001f1P_50K () : unit = runPoissonEvolution mp_d200k1e01g01a0001f1P 50_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01a0001f1P_100K () : unit = runPoissonEvolution mp_d200k1e01g01a0001f1P 100_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01a0001f1E_50K () : unit = runPoissonEvolution mp_d200k1e01g01a0001f1E 50_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01a0001f1E_100K () : unit = runPoissonEvolution mp_d200k1e01g01a0001f1E 100_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01a0001f1E_200K () : unit = runPoissonEvolution mp_d200k1e01g01a0001f1E 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01a0001f1E_500K () : unit = runPoissonEvolution mp_d200k1e01g01a0001f1E 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01a0001f1E_1M () : unit = runPoissonEvolution mp_d200k1e01g01a0001f1E 1_000_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01a0001f1E_2M () : unit = runPoissonEvolution mp_d200k1e01g01a0001f1E 2_000_000 (t.getCallerName())

    // [<Fact>]
    // member t.d200k1e01g01_1M () : unit = runPoissonEvolution mp_d200k1e01g01 1_000_000 (t.getCallerName())

    // // Larger k0.
    //
    // [<Fact>]
    // member t.d100k10e01g01_10K () : unit = runPoissonEvolution mp_d100k10e01g01 10_000 (t.getCallerName())
    //
    // [<Fact>]
    // member t.d200k10e01g01_10K () : unit = runPoissonEvolution mp_d200k10e01g01 10_000 (t.getCallerName())
    //
    // [<Fact>]
    // member t.d200k10e01g01_100K () : unit = runPoissonEvolution mp_d200k10e01g01 100_000 (t.getCallerName())
    //
    // [<Fact>]
    // member t.d200k10e01g01_1M () : unit = runPoissonEvolution mp_d200k10e01g01 1_000_000 (t.getCallerName())

    // ===================================================================================

    // // Quadratic with small linear factor in inf space.
    // [<Fact>]
    // member t.d200k1e01g01i01_10K () : unit = runPoissonEvolution mp_d200k1e01g01i01 10_000 (t.getCallerName())
    //
    // [<Fact>]
    // member t.d200k1e01g01i01_100K () : unit = runPoissonEvolution mp_d200k1e01g01i01 100_000 (t.getCallerName())
    //
    // [<Fact>]
    // member t.d100k10e01g01i01_10K () : unit = runPoissonEvolution mp_d100k10e01g01i01 10_000 (t.getCallerName())
    //
    // [<Fact>]
    // member t.d200k10e01g01i01_10K () : unit = runPoissonEvolution mp_d200k10e01g01i01 10_000 (t.getCallerName())
    //
    // [<Fact>]
    // member t.d200k10e01g01i01_100K () : unit = runPoissonEvolution mp_d200k10e01g01i01 100_000 (t.getCallerName())
    //
    // // Quadratic with larger linear factor in inf space.
    // [<Fact>]
    // member t.d200k10e01g01i1_10K () : unit = runPoissonEvolution mp_d200k10e01g01i1 10_000 (t.getCallerName())
    //
    // [<Fact>]
    // member t.d200k10e01g01i1_100K () : unit = runPoissonEvolution mp_d200k10e01g01i1 100_000 (t.getCallerName())

    // ===================================================================================

    // // Quadratic with even larger linear factor in inf space.
    // [<Fact>]
    // member t.d200k10e01g01i10_10K () : unit = runPoissonEvolution mp_d200k10e01g01i10 10_000 (t.getCallerName())
    //
    // [<Fact>]
    // member t.d200k10e01g01i10_100K () : unit = runPoissonEvolution mp_d200k10e01g01i10 100_000 (t.getCallerName())

    // ===================================================================================

    // [<Fact>]
    // member t.d200k10e01g01i1f1T_100K () : unit = runPoissonEvolution mp_d200k10e01g01i1f1T 100_000 (t.getCallerName())
    //
    //
    // [<Fact>]
    // member t.d200k10e01g01i10f1T_100K () : unit = runPoissonEvolution mp_d200k10e01g01i10f1T 100_000 (t.getCallerName())

    // ===================================================================================

    // [<Fact>]
    // member t.d200k10e01g01i1f1P_100K () : unit = runPoissonEvolution mp_d200k10e01g01i1f1P 100_000 (t.getCallerName())
    //
    //
    // [<Fact>]
    // member t.d200k10e01g01i10f1P_100K () : unit = runPoissonEvolution mp_d200k10e01g01i10f1P 100_000 (t.getCallerName())

    // ===================================================================================

    // [<Fact>]
    // member t.d200k10e01g01i1f1E_100K () : unit = runPoissonEvolution mp_d200k10e01g01i1f1E 100_000 (t.getCallerName())
    //
    //
    // [<Fact>]
    // member t.d200k10e01g01i10f1E_100K () : unit = runPoissonEvolution mp_d200k10e01g01i10f1E 100_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // e = 0.005, a = 0.0001

    [<Fact>]
    member t.d200k1e005g01a0001_200K () : unit = runPoissonEvolution mp_d200k1e005g01a0001 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001f1T_200K () : unit = runPoissonEvolution mp_d200k1e005g01a0001f1T 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001f1P_200K () : unit = runPoissonEvolution mp_d200k1e005g01a0001f1P 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001f1E_200K () : unit = runPoissonEvolution mp_d200k1e005g01a0001f1E 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001_500K () : unit = runPoissonEvolution mp_d200k1e005g01a0001 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001f1T_500K () : unit = runPoissonEvolution mp_d200k1e005g01a0001f1T 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001f1P_500K () : unit = runPoissonEvolution mp_d200k1e005g01a0001f1P 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001f1E_500K () : unit = runPoissonEvolution mp_d200k1e005g01a0001f1E 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001f1E_2M () : unit = runPoissonEvolution mp_d200k1e005g01a0001f1E 2_000_000 (t.getCallerName())

    // ===================================================================================

    // e = 0.005, a = 0.0001, i = 0.1

    [<Fact>]
    member t.d200k1e005g01a0001i1_200K () : unit = runPoissonEvolution mp_d200k1e005g01a0001i1 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001i1f1T_200K () : unit = runPoissonEvolution mp_d200k1e005g01a0001i1f1T 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001i1f1P200K () : unit = runPoissonEvolution mp_d200k1e005g01a0001i1f1P 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001i1f1E_200K () : unit = runPoissonEvolution mp_d200k1e005g01a0001i1f1E 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001i1_500K () : unit = runPoissonEvolution mp_d200k1e005g01a0001i1 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001i1f1T_500K () : unit = runPoissonEvolution mp_d200k1e005g01a0001i1f1T 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001i1f1P_500K () : unit = runPoissonEvolution mp_d200k1e005g01a0001i1f1P 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001i1f1E_500K () : unit = runPoissonEvolution mp_d200k1e005g01a0001i1f1E 500_000 (t.getCallerName())

    // ===================================================================================

    // e = 0.005, a = 0.0001, i = 1.0

    [<Fact>]
    member t.d200k1e005g01a0001i10_200K () : unit = runPoissonEvolution mp_d200k1e005g01a0001i10 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001i10f1T_200K () : unit = runPoissonEvolution mp_d200k1e005g01a0001i10f1T 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001i10f1P200K () : unit = runPoissonEvolution mp_d200k1e005g01a0001i10f1P 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001i10f1E_200K () : unit = runPoissonEvolution mp_d200k1e005g01a0001i10f1E 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001i10_500K () : unit = runPoissonEvolution mp_d200k1e005g01a0001i10 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001i10f1T_500K () : unit = runPoissonEvolution mp_d200k1e005g01a0001i10f1T 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001i10f1P_500K () : unit = runPoissonEvolution mp_d200k1e005g01a0001i10f1P 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001i10f1E_500K () : unit = runPoissonEvolution mp_d200k1e005g01a0001i10f1E 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a0001i10f1E_2M () : unit = runPoissonEvolution mp_d200k1e005g01a0001i10f1E 2_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // e = 0.005, a = 0.001

    [<Fact>]
    member t.d200k1e005g01a001_200K () : unit = runPoissonEvolution mp_d200k1e005g01a001 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001f1T_200K () : unit = runPoissonEvolution mp_d200k1e005g01a001f1T 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001f1P_200K () : unit = runPoissonEvolution mp_d200k1e005g01a001f1P 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001f1E_200K () : unit = runPoissonEvolution mp_d200k1e005g01a001f1E 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001_500K () : unit = runPoissonEvolution mp_d200k1e005g01a001 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001f1T_500K () : unit = runPoissonEvolution mp_d200k1e005g01a001f1T 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001f1P_500K () : unit = runPoissonEvolution mp_d200k1e005g01a001f1P 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001f1E_500K () : unit = runPoissonEvolution mp_d200k1e005g01a001f1E 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001f1E_2M () : unit = runPoissonEvolution mp_d200k1e005g01a001f1E 2_000_000 (t.getCallerName())

    // ===================================================================================

    // e = 0.005, a = 0.001, i = 0.1

    [<Fact>]
    member t.d200k1e005g01a001i1_200K () : unit = runPoissonEvolution mp_d200k1e005g01a001i1 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001i1f1T_200K () : unit = runPoissonEvolution mp_d200k1e005g01a001i1f1T 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001i1f1P200K () : unit = runPoissonEvolution mp_d200k1e005g01a001i1f1P 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001i1f1E_200K () : unit = runPoissonEvolution mp_d200k1e005g01a001i1f1E 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001i1_500K () : unit = runPoissonEvolution mp_d200k1e005g01a001i1 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001i1f1T_500K () : unit = runPoissonEvolution mp_d200k1e005g01a001i1f1T 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001i1f1P_500K () : unit = runPoissonEvolution mp_d200k1e005g01a001i1f1P 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001i1f1E_500K () : unit = runPoissonEvolution mp_d200k1e005g01a001i1f1E 500_000 (t.getCallerName())

    // ===================================================================================

    // e = 0.005, a = 0.001, i = 1.0

    [<Fact>]
    member t.d200k1e005g01a001i10_200K () : unit = runPoissonEvolution mp_d200k1e005g01a001i10 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001i10f1T_200K () : unit = runPoissonEvolution mp_d200k1e005g01a001i10f1T 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001i10f1P200K () : unit = runPoissonEvolution mp_d200k1e005g01a001i10f1P 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001i10f1E_200K () : unit = runPoissonEvolution mp_d200k1e005g01a001i10f1E 200_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001i10_500K () : unit = runPoissonEvolution mp_d200k1e005g01a001i10 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001i10f1T_500K () : unit = runPoissonEvolution mp_d200k1e005g01a001i10f1T 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001i10f1P_500K () : unit = runPoissonEvolution mp_d200k1e005g01a001i10f1P 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001i10f1E_500K () : unit = runPoissonEvolution mp_d200k1e005g01a001i10f1E 500_000 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01a001i10f1E_2M () : unit = runPoissonEvolution mp_d200k1e005g01a001i10f1E 2_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // D = 300, e = 0.005, a = 0.0001

    [<Fact>]
    member t.d300k1e005g01a0001_200K () : unit = runPoissonEvolution mp_d300k1e005g01a0001 200_000 (t.getCallerName())

    [<Fact>]
    member t.d300k1e005g01a0001f1T_200K () : unit = runPoissonEvolution mp_d300k1e005g01a0001f1T 200_000 (t.getCallerName())

    [<Fact>]
    member t.d300k1e005g01a0001f1P_200K () : unit = runPoissonEvolution mp_d300k1e005g01a0001f1P 200_000 (t.getCallerName())

    [<Fact>]
    member t.d300k1e005g01a0001f1E_200K () : unit = runPoissonEvolution mp_d300k1e005g01a0001f1E 200_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // D = 500, e = 0.005, a = 0.0001

    /// Performance test.
    [<Fact>]
    member t.d500k1e005g01a0001_500 () : unit = runPoissonEvolution mp_d500k1e005g01a0001 500 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a0001_200K () : unit = runPoissonEvolution mp_d500k1e005g01a0001 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a0001f1T_200K () : unit = runPoissonEvolution mp_d500k1e005g01a0001f1T 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a0001f1P_200K () : unit = runPoissonEvolution mp_d500k1e005g01a0001f1P 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a0001f1E_200K () : unit = runPoissonEvolution mp_d500k1e005g01a0001f1E 200_000 (t.getCallerName())


    [<Fact>]
    member t.d500k1e005g01a0001f1E_500K () : unit = runPoissonEvolution mp_d500k1e005g01a0001f1E 500_000 (t.getCallerName())

    [<Fact>]
    member private t.d500k1e005g01a0001f1E_1M () : unit = runPoissonEvolution mp_d500k1e005g01a0001f1E 1_000_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a0001f1E_2M () : unit = runPoissonEvolution mp_d500k1e005g01a0001f1E 2_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // D = 500, e = 0.01, a = 0.0001

    [<Fact>]
    member t.d500k1e01g01a0001_200K () : unit = runPoissonEvolution mp_d500k1e01g01a0001 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a0001f1T_200K () : unit = runPoissonEvolution mp_d500k1e01g01a0001f1T 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a0001f1P_200K () : unit = runPoissonEvolution mp_d500k1e01g01a0001f1P 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a0001f1E_200K () : unit = runPoissonEvolution mp_d500k1e01g01a0001f1E 200_000 (t.getCallerName())


    [<Fact>]
    member t.d500k1e01g01a0001f1E_500K () : unit = runPoissonEvolution mp_d500k1e01g01a0001f1E 500_000 (t.getCallerName())

    [<Fact>]
    member private t.d500k1e01g01a0001f1E_1M () : unit = runPoissonEvolution mp_d500k1e01g01a0001f1E 1_000_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a0001f1E_2M () : unit = runPoissonEvolution mp_d500k1e01g01a0001f1E 2_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // D = 500, e = 0.005, a = 0.001

    [<Fact>]
    member t.d500k1e005g01a001_200K () : unit = runPoissonEvolution mp_d500k1e005g01a001 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a001f1T_200K () : unit = runPoissonEvolution mp_d500k1e005g01a001f1T 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a001f1P_200K () : unit = runPoissonEvolution mp_d500k1e005g01a001f1P 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a001f1E_200K () : unit = runPoissonEvolution mp_d500k1e005g01a001f1E 200_000 (t.getCallerName())


    [<Fact>]
    member t.d500k1e005g01a001f1E_500K () : unit = runPoissonEvolution mp_d500k1e005g01a001f1E 500_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a001f1E_1M () : unit = runPoissonEvolution mp_d500k1e005g01a001f1E 1_000_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a001f1E_2M () : unit = runPoissonEvolution mp_d500k1e005g01a001f1E 2_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // D = 500, e = 0.005, a = 0.002

    [<Fact>]
    member t.d500k1e005g01a002f1E_200K () : unit = runPoissonEvolution mp_d500k1e005g01a002f1E 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a002f1E_1M () : unit = runPoissonEvolution mp_d500k1e005g01a002f1E 1_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // D = 500, e = 0.005, a = 0.005

    [<Fact>]
    member t.d500k1e005g01a005f1E_200K () : unit = runPoissonEvolution mp_d500k1e005g01a005f1E 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a005f1E_1M () : unit = runPoissonEvolution mp_d500k1e005g01a005f1E 1_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // D = 500, e = 0.005, a = 0.002, i = 1.0

    [<Fact>]
    member t.d500k1e005g01a002i10f1E_200K () : unit = runPoissonEvolution mp_d500k1e005g01a002i10f1E 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a002i10f1E_1M () : unit = runPoissonEvolution mp_d500k1e005g01a002i10f1E 1_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // D = 500, e = 0.005, a = 0.005, i = 1.0

    [<Fact>]
    member t.d500k1e005g01a005i10f1E_200K () : unit = runPoissonEvolution mp_d500k1e005g01a005i10f1E 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a005i10f1E_1M () : unit = runPoissonEvolution mp_d500k1e005g01a005i10f1E 1_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // D = 500, e = 0.01, a = 0.001

    [<Fact>]
    member t.d500k1e01g01a001_200K () : unit = runPoissonEvolution mp_d500k1e01g01a001 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a001f1T_200K () : unit = runPoissonEvolution mp_d500k1e01g01a001f1T 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a001f1P_200K () : unit = runPoissonEvolution mp_d500k1e01g01a001f1P 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a001f1E_200K () : unit = runPoissonEvolution mp_d500k1e01g01a001f1E 200_000 (t.getCallerName())


    [<Fact>]
    member t.d500k1e01g01a001f1E_500K () : unit = runPoissonEvolution mp_d500k1e01g01a001f1E 500_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a001f1E_1M () : unit = runPoissonEvolution mp_d500k1e01g01a001f1E 1_000_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a001f1E_2M () : unit = runPoissonEvolution mp_d500k1e01g01a001f1E 2_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // D = 500, e = 0.01, a = 0.002

    [<Fact>]
    member t.d500k1e01g01a002f1E_200K () : unit = runPoissonEvolution mp_d500k1e01g01a002f1E 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a002f1E_1M () : unit = runPoissonEvolution mp_d500k1e01g01a002f1E 1_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // D = 500, e = 0.01, a = 0.005

    [<Fact>]
    member t.d500k1e01g01a005f1E_200K () : unit = runPoissonEvolution mp_d500k1e01g01a005f1E 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a005f1E_1M () : unit = runPoissonEvolution mp_d500k1e01g01a005f1E 1_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // D = 500, e = 0.01, a = 0.002, i = 1.0

    [<Fact>]
    member t.d500k1e01g01a002i10f1E_200K () : unit = runPoissonEvolution mp_d500k1e01g01a002i10f1E 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a002i10f1E_1M () : unit = runPoissonEvolution mp_d500k1e01g01a002i10f1E 1_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // D = 500, e = 0.01, a = 0.005, i = 1.0

    [<Fact>]
    member t.d500k1e01g01a005i10f1E_200K () : unit = runPoissonEvolution mp_d500k1e01g01a005i10f1E 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a005i10f1E_1M () : unit = runPoissonEvolution mp_d500k1e01g01a005i10f1E 1_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    [<Fact>]
    member t.d500_round_1() : unit =
        let tests =
            [
                async { t.d500k1e005g01a0001_200K() }
                async { t.d500k1e005g01a0001f1T_200K() }
                async { t.d500k1e005g01a0001f1P_200K() }
                async { t.d500k1e005g01a0001f1E_200K() }
                async { t.d500k1e005g01a001_200K() }
                async { t.d500k1e005g01a001f1T_200K() }
                async { t.d500k1e005g01a001f1P_200K() }
                async { t.d500k1e005g01a001f1E_200K() }
            ]

        Async.Parallel tests
        |> Async.RunSynchronously
        |> ignore


    // ===================================================================================
    // ===================================================================================

    // D = 500, e = 0.005, a = 0.0001, i = 1.0

    [<Fact>]
    member t.d500k1e005g01a0001i10_200K () : unit = runPoissonEvolution mp_d500k1e005g01a0001i10 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a0001i10f1T_200K () : unit = runPoissonEvolution mp_d500k1e005g01a0001i10f1T 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a0001i10f1P_200K () : unit = runPoissonEvolution mp_d500k1e005g01a0001i10f1P 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a0001i10f1E_200K () : unit = runPoissonEvolution mp_d500k1e005g01a0001i10f1E 200_000 (t.getCallerName())


    [<Fact>]
    member t.d500k1e005g01a0001i10f1E_500K () : unit = runPoissonEvolution mp_d500k1e005g01a0001i10f1E 500_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a0001i10f1E_1M () : unit = runPoissonEvolution mp_d500k1e005g01a0001i10f1E 1_000_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a0001i10f1E_2M () : unit = runPoissonEvolution mp_d500k1e005g01a0001i10f1E 2_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // D = 500, e = 0.01, a = 0.0001, i = 1.0

    [<Fact>]
    member t.d500k1e01g01a0001i10_200K () : unit = runPoissonEvolution mp_d500k1e01g01a0001i10 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a0001i10f1T_200K () : unit = runPoissonEvolution mp_d500k1e01g01a0001i10f1T 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a0001i10f1P_200K () : unit = runPoissonEvolution mp_d500k1e01g01a0001i10f1P 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a0001i10f1E_200K () : unit = runPoissonEvolution mp_d500k1e01g01a0001i10f1E 200_000 (t.getCallerName())


    [<Fact>]
    member t.d500k1e01g01a0001i10f1E_500K () : unit = runPoissonEvolution mp_d500k1e01g01a0001i10f1E 500_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a0001i10f1E_1M () : unit = runPoissonEvolution mp_d500k1e01g01a0001i10f1E 1_000_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a0001i10f1E_2M () : unit = runPoissonEvolution mp_d500k1e01g01a0001i10f1E 2_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // D = 500, e = 0.005, a = 0.001, i = 1.0

    [<Fact>]
    member t.d500k1e005g01a001i10_200K () : unit = runPoissonEvolution mp_d500k1e005g01a001i10 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a001i10f1T_200K () : unit = runPoissonEvolution mp_d500k1e005g01a001i10f1T 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a001i10f1P_200K () : unit = runPoissonEvolution mp_d500k1e005g01a001i10f1P 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a001i10f1E_200K () : unit = runPoissonEvolution mp_d500k1e005g01a001i10f1E 200_000 (t.getCallerName())


    [<Fact>]
    member t.d500k1e005g01a001i10f1E_500K () : unit = runPoissonEvolution mp_d500k1e005g01a001i10f1E 500_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a001i10f1E_1M () : unit = runPoissonEvolution mp_d500k1e005g01a001i10f1E 1_000_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e005g01a001i10f1E_2M () : unit = runPoissonEvolution mp_d500k1e005g01a001i10f1E 2_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // D = 500, e = 0.01, a = 0.001, i = 1.0

    [<Fact>]
    member t.d500k1e01g01a001i10_200K () : unit = runPoissonEvolution mp_d500k1e01g01a001i10 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a001i10f1T_200K () : unit = runPoissonEvolution mp_d500k1e01g01a001i10f1T 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a001i10f1P_200K () : unit = runPoissonEvolution mp_d500k1e01g01a001i10f1P 200_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a001i10f1E_200K () : unit = runPoissonEvolution mp_d500k1e01g01a001i10f1E 200_000 (t.getCallerName())


    [<Fact>]
    member t.d500k1e01g01a001i10f1E_500K () : unit = runPoissonEvolution mp_d500k1e01g01a001i10f1E 500_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a001i10f1E_1M () : unit = runPoissonEvolution mp_d500k1e01g01a001i10f1E 1_000_000 (t.getCallerName())

    [<Fact>]
    member t.d500k1e01g01a001i10f1E_2M () : unit = runPoissonEvolution mp_d500k1e01g01a001i10f1E 2_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    [<Fact>]
    member t.d500_round_2() : unit =
        let tests =
            [
                async { t.d500k1e005g01a0001i10_200K() }
                async { t.d500k1e005g01a0001i10f1T_200K() }
                async { t.d500k1e005g01a0001i10f1P_200K() }
                async { t.d500k1e005g01a0001i10f1E_200K() }
                async { t.d500k1e005g01a001i10_200K() }
                async { t.d500k1e005g01a001i10f1T_200K() }
                async { t.d500k1e005g01a001i10f1P_200K() }
                async { t.d500k1e005g01a001i10f1E_200K() }
            ]

        Async.Parallel tests
        |> Async.RunSynchronously
        |> ignore

    // ===================================================================================
    // ===================================================================================

    [<Fact>]
    member t.d500_round_3() : unit =
        let tests =
            [
                async { t.d500k1e005g01a0001f1E_500K() }
                async { t.d500k1e005g01a0001f1E_1M() }
                async { t.d500k1e005g01a0001f1E_2M() }
                async { t.d500k1e005g01a001f1E_500K() }
                async { t.d500k1e005g01a001f1E_1M() }
                async { t.d500k1e005g01a001f1E_2M() }
                async { t.d500k1e005g01a0001i10f1E_500K() }
                async { t.d500k1e005g01a0001i10f1E_1M() }
                async { t.d500k1e005g01a0001i10f1E_2M() }
                async { t.d500k1e005g01a001i10f1E_500K() }
                async { t.d500k1e005g01a001i10f1E_1M() }
                async { t.d500k1e005g01a001i10f1E_2M() }
            ]

        Async.Parallel tests
        |> Async.RunSynchronously
        |> ignore

    [<Fact>]
    member t.d500_round_1_3() : unit =
        let tests =
            [
                // d = 100
                async { t.d100k01e01a0_100K () }

                // d = 200
                async { t.d200k1e005g01a0001f1E_200K () }
                async { t.d200k1e005g01a0001f1E_500K () }
                async { t.d200k1e005g01a0001f1E_2M () }

                // d = 500
                async { t.d500k1e005g01a0001_200K() }
                async { t.d500k1e005g01a0001f1T_200K() }
                async { t.d500k1e005g01a0001f1P_200K() }
                async { t.d500k1e005g01a0001f1E_200K() }
                async { t.d500k1e005g01a001_200K() }
                async { t.d500k1e005g01a001f1T_200K() }
                async { t.d500k1e005g01a001f1P_200K() }
                async { t.d500k1e005g01a001f1E_200K() }

                async { t.d500k1e005g01a0001i10_200K() }
                async { t.d500k1e005g01a0001i10f1T_200K() }
                async { t.d500k1e005g01a0001i10f1P_200K() }
                async { t.d500k1e005g01a0001i10f1E_200K() }
                async { t.d500k1e005g01a001i10_200K() }
                async { t.d500k1e005g01a001i10f1T_200K() }
                async { t.d500k1e005g01a001i10f1P_200K() }
                async { t.d500k1e005g01a001i10f1E_200K() }

                async { t.d500k1e005g01a0001f1E_500K() }
                async { t.d500k1e005g01a0001f1E_1M() }
                async { t.d500k1e005g01a0001f1E_2M() }
                async { t.d500k1e005g01a001f1E_500K() }
                async { t.d500k1e005g01a001f1E_1M() }
                async { t.d500k1e005g01a001f1E_2M() }
                async { t.d500k1e005g01a0001i10f1E_500K() }
                async { t.d500k1e005g01a0001i10f1E_1M() }
                async { t.d500k1e005g01a0001i10f1E_2M() }
                async { t.d500k1e005g01a001i10f1E_500K() }
                async { t.d500k1e005g01a001i10f1E_1M() }
                async { t.d500k1e005g01a001i10f1E_2M() }
            ]

        Async.Parallel tests
        |> Async.RunSynchronously
        |> ignore


    [<Fact>]
    member t.d500_round_1_4() : unit =
        let tests =
            [
                // d = 500
                async { t.d500k1e01g01a0001_200K() }
                async { t.d500k1e01g01a0001f1E_200K() }
                async { t.d500k1e01g01a001_200K() }
                async { t.d500k1e01g01a001f1E_200K() }

                async { t.d500k1e01g01a0001i10_200K() }
                async { t.d500k1e01g01a0001i10f1E_200K() }
                async { t.d500k1e01g01a001i10_200K() }
                async { t.d500k1e01g01a001i10f1E_200K() }

                async { t.d500k1e01g01a0001f1E_1M() }
                async { t.d500k1e01g01a0001f1E_2M() }
                async { t.d500k1e01g01a001f1E_1M() }
                async { t.d500k1e01g01a001f1E_2M() }
                async { t.d500k1e01g01a0001i10f1E_1M() }
                async { t.d500k1e01g01a0001i10f1E_2M() }
                async { t.d500k1e01g01a001i10f1E_1M() }
                async { t.d500k1e01g01a001i10f1E_2M() }
            ]

        Async.Parallel tests
        |> Async.RunSynchronously
        |> ignore


    [<Fact>]
    member t.d500_round_1_5() : unit =
        let tests =
            [
                async { t.d500k1e005g01a002f1E_200K() }
                async { t.d500k1e005g01a002f1E_1M() }

                async { t.d500k1e005g01a005f1E_200K() }
                async { t.d500k1e005g01a005f1E_1M() }

                async { t.d500k1e01g01a002f1E_200K() }
                async { t.d500k1e01g01a002f1E_1M() }

                async { t.d500k1e01g01a005f1E_200K() }
                async { t.d500k1e01g01a005f1E_1M() }

                async { t.d500k1e005g01a002i10f1E_200K() }
                async { t.d500k1e005g01a002i10f1E_1M() }

                async { t.d500k1e005g01a005i10f1E_200K() }
                async { t.d500k1e005g01a005i10f1E_1M() }

                async { t.d500k1e01g01a002i10f1E_200K() }
                async { t.d500k1e01g01a002i10f1E_1M() }

                async { t.d500k1e01g01a005i10f1E_200K() }
                async { t.d500k1e01g01a005i10f1E_1M() }
            ]

        Async.Parallel tests
        |> Async.RunSynchronously
        |> ignore


    [<Fact>]
    member t.d500_round_1_6() : unit =
        let tests =
            [
                async { t.d500k1e01g01a005i10f1E_200K() }
                async { t.d500k1e01g01a005i10f1E_1M() }
            ]

        Async.Parallel tests
        |> Async.RunSynchronously
        |> ignore


    [<Fact>]
    member t.generateModel_shouldWork() : unit =
        let name = t.getCallerName()
        let mp = mp_d100k10e01g01i1
        let noOfEpochs = NoOfEpochs 100_000
        let p = PoissonParam.defaultValue mp noOfEpochs name
        let i = p.initialData
        let systemProxy = ModelGeneratorSystemProxy.create()
        let result = poissonModelGenerator systemProxy i
        writeLine $"result: '%A{result}'."

        match result with
        | Ok _ -> ()
        | Error e -> failwith $"Error: '{e}'."
