namespace FredholmSolverTests

open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FredholmSolver.EeInfCharts
open Primitives.GeneralData
open System
open FluentAssertions
open FluentAssertions.Execution
open GenericOdeSolver.Primitives
open GenericOdeSolver.Solver
open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives
open FredholmSolver.Primitives
open FredholmSolver.Kernel
open FredholmSolver
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
open FredholmSolverTests.PoissonTestData

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
    let errInvTolerance = 1.0e-3
    let outputFolder = @"C:\EeInf"
    let noOfCharts = 20

    let createModel (mp : EeInfIntModelParams) name =
        let md = mp.named name
        let model = EeInfIntModel.create md
        model

    let getChartInitData model noOfEpochs =
        {
            baseData =
                {
                    resultId = RunQueueId.getNewId()
                    modelParams = model.intModelParams.eeInfModelParams
                    domain2D = model.kernelData.domain2D
                }
            totalMolecules = model.intModelParams.intInitParams.totalMolecules
            noOfEpochs = noOfEpochs
        }

    let getChartSliceData model noOfEpochs chartMod e i : ChartSliceIntData =
        {
            epochNumber = i
            progress = (decimal i) / (decimal noOfEpochs)
            statData  = calculateIntStat model e
            substanceData  = if i % chartMod = 0 then Some e else None
        }

    let outputChart (chartData : ChartIntData) =
        writeLine "epochNumber,food,waste,total,invariant,eeMean,eeStdDev,infMean,infStdDev"

        chartData.allChartData
        |> List.rev
        |> List.map (fun e -> writeLine $"{e.epochNumber},{e.statData.food},{e.statData.waste},{e.statData.total},{e.statData.invariant},{e.statData.eeStatData.mean},{e.statData.eeStatData.stdDev},{e.statData.infStatData.mean},{e.statData.infStatData.stdDev}")
        |> ignore

    let toWolframData (model : EeInfIntModel) noOfEpochs (substanceData : SubstanceIntData) (chartData : ChartIntData) =
        let a = $"""Get["C:\\GitHub\\CoreClm\\Math\\odePackChartSupport.m"];{Nl}{Nl}"""
        let b = $"""plotAll[1];{Nl}"""
        let d = model.intModelParams |> toOutputString |> toWolframNotation
        let descr = $"descr ={Nl}\"{d}{Nl}noOfEpochs = {noOfEpochs}\";{Nl}{Nl}"
        let gamma0 = model.intModelParams.eeInfModelParams.gammaFuncValue.gamma0.value
        let k0 = model.intModelParams.eeInfModelParams.kernelParams.kaFuncValue.k0.value
        let totalMolecules = model.intModelParams.intInitParams.totalMolecules.value
        let norm = 100.0 / (double totalMolecules) // Use values in %.

        let eta = model.kernelData.domain2D.eeDomain.points.value
        let zeta = model.kernelData.domain2D.infDomain.points.value
        let etaData = $"etaData = {(toWolframNotation eta)};{Nl}{Nl}"
        let zetaData = $"zetaData = {(toWolframNotation zeta)};{Nl}{Nl}"
        let k0Data = $"k0 = {(toWolframNotation k0)};{Nl}{Nl}"
        let gamma0Data = $"gamma0 = {(toWolframNotation gamma0)};{Nl}{Nl}"

        let u = (norm * (substanceData.protocell.value.convert double)).value
        let uData = $"uData = {(toWolframNotation u)};{Nl}{Nl}"

        // Need to rescale ka back.
        let n = model.intModelParams.eeInfModelParams.numberOfMolecules.value
        let kMult = pown (double totalMolecules) n
        let ka = model.kernelData.ka.value.value |> Array.map (fun a -> a |> Array.map (fun b -> b * kMult / k0))

        let kaData = $"ka = {(toWolframNotation ka)};{Nl}{Nl}"

        let gamma = model.gamma.value.value |> Array.map (fun a -> a |> Array.map (fun b -> b / gamma0))
        let gammaData = $"gamma = {(toWolframNotation gamma)};{Nl}{Nl}"

        let w (e : ChartSliceIntData) =
            let g x = toWolframNotation x

            let a =
                [
                    $"{(g (int e.epochNumber))}"
                    $"{g e.statData.eeStatData.mean}"
                    $"{g e.statData.eeStatData.stdDev}"
                    $"{g e.statData.infStatData.mean}"
                    $"{g e.statData.infStatData.stdDev}"
                    $"{g (norm * (double e.statData.invariant))}"
                    $"{g (norm * (double e.statData.total))}"
                    $"{g (norm * (double e.statData.food))}"
                    $"{g (norm * (double e.statData.waste))}"
                    $"{g e.progress}"
                ]
                |> joinStrings ", "

            $"{{ {a} }}"

        let chartTitles = $"{Nl}chartTitles = {{\"epochNumber\", \"eeMean\", \"eeStdDev\", \"infMean\", \"infStdDev\", \"invariant\", \"total\", \"food\", \"waste\", \"progress\"}};{Nl}"

        let chart =
            chartData.allChartData
            |> List.sortBy (fun e -> e.epochNumber)
            |> List.map w
            |> joinStrings $",{Nl}"

        let chartDataStr = $"chartData = {{ {chart} }};{Nl}{Nl}"

        let uw (e : ChartSliceIntData) =
            match e.substanceData with
            | Some v ->
                let t = toWolframNotation (int e.epochNumber)
                let u = (norm * (v.protocell.value.convert double)).value |> toWolframNotation
                $"{{ {t}, {u} }}" |> Some
            | None -> None

        let ut =
            chartData.allChartData
            |> List.sortBy (fun e -> e.epochNumber)
            |> List.map uw
            |> List.choose id
            |> joinStrings $",{Nl}"

        let uDataT = $"uDataT = {{ {ut} }};{Nl}{Nl}"

        $"{a}{descr}{k0Data}{gamma0Data}{etaData}{zetaData}{kaData}{gammaData}{uData}{chartTitles}{chartDataStr}{uDataT}{b}"

    let runPoissonEvolution mp noOfEpochs name =
        let model = createModel mp name
        let initialValue = model.intInitialValues
        let startInv = model.invariant initialValue
        let startStat = calculateIntStat model initialValue
        let ps = Random 1 |> PoissonSampler.create
        let chartMod = noOfEpochs / noOfCharts
        let chartFrequency = if noOfEpochs <= 100_000 then 1 else noOfEpochs / 100_000

        let chartInitData = getChartInitData model (NoOfEpochs noOfEpochs)
        let chartDataUpdater = AsyncChartIntDataUpdater(ChartIntDataUpdater(), chartInitData)
        let getChartSliceData = getChartSliceData model noOfEpochs chartMod

        let evolve e i =
            let e1 = model.evolve ps e
            if i % chartFrequency = 0 then getChartSliceData e1 i |> chartDataUpdater.addContent
            e1

        let result = [|for i in 0..noOfEpochs -> i |] |> Array.fold evolve initialValue
        let endInv = model.invariant result
        let endStat = calculateIntStat model result
        let chartData = chartDataUpdater.getContent()
        chartData |> outputChart

        let wolframFileName = $@"{outputFolder}\{name}.m"
        let wolframData = toWolframData model noOfEpochs result chartData
        File.WriteAllText(wolframFileName, wolframData)

        use _ = new AssertionScope()
        writeLine $"noOfEpochs = {noOfEpochs}, noOfDomainPoints = {mp.eeInfModelParams.kernelParams.domainIntervals}"
        writeLine $"startInv = {startInv}, endInv = {endInv}"
        writeLine $"start: food = {startStat.food}, waste = {startStat.waste}, u = {startStat.total}"
        writeLine $"start: ee mean = {startStat.eeStatData.mean}, ee stdDev = {startStat.eeStatData.stdDev}, inf mean = {startStat.infStatData.mean}, inf stdDev = {startStat.infStatData.stdDev}"
        writeLine $"end: food = {endStat.food}, waste = {endStat.waste}, u = {endStat.total}"
        writeLine $"end: ee mean = {endStat.eeStatData.mean}, ee stdDev = {endStat.eeStatData.stdDev}, inf mean = {endStat.infStatData.mean}, inf stdDev = {endStat.infStatData.stdDev}"
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

    member private t.d500k1e005g01a0001_200K () : unit = runPoissonEvolution mp_d500k1e005g01a0001 200_000 (t.getCallerName())
    member private t.d500k1e005g01a0001f1T_200K () : unit = runPoissonEvolution mp_d500k1e005g01a0001f1T 200_000 (t.getCallerName())
    member private t.d500k1e005g01a0001f1P_200K () : unit = runPoissonEvolution mp_d500k1e005g01a0001f1P 200_000 (t.getCallerName())
    member private t.d500k1e005g01a0001f1E_200K () : unit = runPoissonEvolution mp_d500k1e005g01a0001f1E 200_000 (t.getCallerName())

    member private t.d500k1e005g01a0001f1E_500K () : unit = runPoissonEvolution mp_d500k1e005g01a0001f1E 500_000 (t.getCallerName())
    member private t.d500k1e005g01a0001f1E_1M () : unit = runPoissonEvolution mp_d500k1e005g01a0001f1E 1_000_000 (t.getCallerName())
    member private t.d500k1e005g01a0001f1E_2M () : unit = runPoissonEvolution mp_d500k1e005g01a0001f1E 2_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // D = 500, e = 0.005, a = 0.001

    member private t.d500k1e005g01a001_200K () : unit = runPoissonEvolution mp_d500k1e005g01a001 200_000 (t.getCallerName())
    member private t.d500k1e005g01a001f1T_200K () : unit = runPoissonEvolution mp_d500k1e005g01a001f1T 200_000 (t.getCallerName())
    member private t.d500k1e005g01a001f1P_200K () : unit = runPoissonEvolution mp_d500k1e005g01a001f1P 200_000 (t.getCallerName())
    member private t.d500k1e005g01a001f1E_200K () : unit = runPoissonEvolution mp_d500k1e005g01a001f1E 200_000 (t.getCallerName())

    member private t.d500k1e005g01a001f1E_500K () : unit = runPoissonEvolution mp_d500k1e005g01a001f1E 500_000 (t.getCallerName())
    member private t.d500k1e005g01a001f1E_1M () : unit = runPoissonEvolution mp_d500k1e005g01a001f1E 1_000_000 (t.getCallerName())
    member private t.d500k1e005g01a001f1E_2M () : unit = runPoissonEvolution mp_d500k1e005g01a001f1E 2_000_000 (t.getCallerName())

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

    member private t.d500k1e005g01a0001i10_200K () : unit = runPoissonEvolution mp_d500k1e005g01a0001i10 200_000 (t.getCallerName())
    member private t.d500k1e005g01a0001i10f1T_200K () : unit = runPoissonEvolution mp_d500k1e005g01a0001i10f1T 200_000 (t.getCallerName())
    member private t.d500k1e005g01a0001i10f1P_200K () : unit = runPoissonEvolution mp_d500k1e005g01a0001i10f1P 200_000 (t.getCallerName())
    member private t.d500k1e005g01a0001i10f1E_200K () : unit = runPoissonEvolution mp_d500k1e005g01a0001i10f1E 200_000 (t.getCallerName())

    member private t.d500k1e005g01a0001i10f1E_500K () : unit = runPoissonEvolution mp_d500k1e005g01a0001i10f1E 500_000 (t.getCallerName())
    member private t.d500k1e005g01a0001i10f1E_1M () : unit = runPoissonEvolution mp_d500k1e005g01a0001i10f1E 1_000_000 (t.getCallerName())
    member private t.d500k1e005g01a0001i10f1E_2M () : unit = runPoissonEvolution mp_d500k1e005g01a0001i10f1E 2_000_000 (t.getCallerName())

    // ===================================================================================
    // ===================================================================================

    // D = 500, e = 0.005, a = 0.001, i = 1.0

    member private t.d500k1e005g01a001i10_200K () : unit = runPoissonEvolution mp_d500k1e005g01a001i10 200_000 (t.getCallerName())
    member private t.d500k1e005g01a001i10f1T_200K () : unit = runPoissonEvolution mp_d500k1e005g01a001i10f1T 200_000 (t.getCallerName())
    member private t.d500k1e005g01a001i10f1P_200K () : unit = runPoissonEvolution mp_d500k1e005g01a001i10f1P 200_000 (t.getCallerName())
    member private t.d500k1e005g01a001i10f1E_200K () : unit = runPoissonEvolution mp_d500k1e005g01a001i10f1E 200_000 (t.getCallerName())

    member private t.d500k1e005g01a001i10f1E_500K () : unit = runPoissonEvolution mp_d500k1e005g01a001i10f1E 500_000 (t.getCallerName())
    member private t.d500k1e005g01a001i10f1E_1M () : unit = runPoissonEvolution mp_d500k1e005g01a001i10f1E 1_000_000 (t.getCallerName())
    member private t.d500k1e005g01a001i10f1E_2M () : unit = runPoissonEvolution mp_d500k1e005g01a001i10f1E 2_000_000 (t.getCallerName())

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
