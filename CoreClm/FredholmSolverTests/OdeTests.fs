﻿namespace FredholmSolverTests

open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FredholmSolver.EeInfCharts
//open Primitives.GeneralData
open System
open FluentAssertions.Execution
open FluentAssertions
//open GenericOdeSolver.Primitives
//open GenericOdeSolver.Solver
//open Primitives.GeneralPrimitives
//open Primitives.SolverPrimitives
open FredholmSolver.Primitives
open FredholmSolver.Kernel
open FredholmSolver.EeInfDiffModel
open FredholmSolver.EeInfChartData
//open Primitives.SolverRunnerErrors
open Softellect.Sys.Core
open Softellect.Sys.Logging
open Xunit
open Xunit.Abstractions
open Plotly.NET
open Analytics.ChartExt
open Primitives.ChartPrimitives
//open Primitives.WolframPrimitives
open Softellect.Sys.Primitives
open Softellect.Analytics.Wolfram
open Softellect.DistributedProcessing.Primitives.Common
open Softellect.DistributedProcessing.SolverRunner.Primitives
open Softellect.Math.Primitives

type CallBackResults =
    {
        progressCallBackCount : int
        completedCallBackCount : int
        cancelledCallBackCount : int
        abortedCallBackCount : int
        chartCallBackCount : int
        chartDetailedCallBackCount : int
    }

    static member defaultValue =
        {
            progressCallBackCount = 0
            completedCallBackCount = 0
            cancelledCallBackCount = 0
            abortedCallBackCount = 0
            chartCallBackCount = 0
            chartDetailedCallBackCount = 0
        }


type OdeTestData =
    {
        startTime : EvolutionTime
        endTime : EvolutionTime
        stepSize : double
        absoluteTolerance : AbsoluteTolerance
        odeSolverType : OdeSolverType
    }


type OdeResultData =
    {
        modelData : EeInfDiffModel
        callBackResults : CallBackResults
        //result : CallBackData
        x : double[]
        getData : double[] -> SubstanceLinearData
        invStart : double
        chartData : ChartDiffData
    }

    member r.toWolframData (odeParams : OdeTestData) =
        let a = $"""Get["C:\\GitHub\\CoreClm\\Math\\odePackChartSupport.m"];{Nl}{Nl}"""
        let b = $"""plotAll[1];{Nl}"""
        let d = r.modelData.diffModelParams |> toOutputString |> toWolframNotation
        let o = odeParams |> toOutputString |> toWolframNotation
        let descr = $"descr ={Nl}\"{d}{Nl}{o}\";{Nl}{Nl}"
        let gamma0 = r.modelData.diffModelParams.eeInfModelParams.gammaFuncValue.gamma0.value
        let k0 = r.modelData.diffModelParams.eeInfModelParams.kernelParams.kaFuncValue.k0.value

        let eta = r.modelData.kernelData.domain2D.eeDomain.points.value
        let zeta = r.modelData.kernelData.domain2D.infDomain.points.value
        let etaData = $"etaData = {(toWolframNotation eta)};{Nl}{Nl}"
        let zetaData = $"zetaData = {(toWolframNotation zeta)};{Nl}{Nl}"
        let k0Data = $"k0 = {(toWolframNotation k0)};{Nl}{Nl}"
        let gamma0Data = $"gamma0 = {(toWolframNotation gamma0)};{Nl}{Nl}"

        let substanceData = r.getData r.x
        let u = substanceData.protocell.toMatrix().value
        let uData = $"uData = {(toWolframNotation u)};{Nl}{Nl}"
        let ka = r.modelData.kernelData.ka.value.value |> Array.map (fun a -> a |> Array.map (fun b -> b / k0))

        let kaData = $"ka = {(toWolframNotation ka)};{Nl}{Nl}"

        let gamma = r.modelData.gamma.value.value |> Array.map (fun a -> a |> Array.map (fun b -> b / gamma0))
        let gammaData = $"gamma = {(toWolframNotation gamma)};{Nl}{Nl}"

        let w (e : ChartSliceDiffData) =
            let g x = toWolframNotation x

            let a =
                [
                    $"{(g e.tChart)}"
                    $"{g e.statData.eeStatData.mean}"
                    $"{g e.statData.eeStatData.stdDev}"
                    $"{g e.statData.infStatData.mean}"
                    $"{g e.statData.infStatData.stdDev}"
                    $"{g e.statData.invariant}"
                    $"{g e.statData.total}"
                    $"{g e.statData.food}"
                    $"{g e.statData.waste}"
                    $"{g e.progressChart.progressInfo.callCount}"
                ]
                |> joinStrings ", "

            $"{{ {a} }}"

        let chartTitles = $"{Nl}chartTitles = {{\"t\", \"eeMean\", \"eeStdDev\", \"infMean\", \"infStdDev\", \"invariant\", \"total\", \"food\", \"waste\", \"callCount\"}};{Nl}"

        let chart =
            r.chartData.allChartData
            |> List.sortBy (fun e -> e.tChart)
            |> List.map w
            |> joinStrings $",{Nl}"

        let chartData = $"chartData = {{ {chart} }};{Nl}{Nl}"

        let uw (e : ChartSliceDiffData) =
            match e.substanceData with
            | Some v ->
                let t = toWolframNotation e.tChart
                let u = v.protocell.toMatrix().value |> toWolframNotation
                $"{{ {t}, {u} }}" |> Some
            | None -> None

        let ut =
            r.chartData.allChartData
            |> List.sortBy (fun e -> e.tChart)
            |> List.map uw
            |> List.choose id
            |> joinStrings $",{Nl}"

        let uDataT = $"uDataT = {{ {ut} }};{Nl}{Nl}"

        $"{a}{descr}{k0Data}{gamma0Data}{etaData}{zetaData}{kaData}{gammaData}{uData}{chartTitles}{chartData}{uDataT}{b}"


type OdeTestInitialData =
    {
        x : int
    }


type OdeTestContext =
    {
        derivativeCalculator : DerivativeCalculator
        evolutionTime : EvolutionTime
        initialValues : double[]
    }


    member d.inputParams =
        {
            startTime = EvolutionTime 0m
            endTime = d.evolutionTime
        }

    member d.odeContext =
        {
            stepSize = 0.0
            absoluteTolerance = AbsoluteTolerance.defaultValue
            odeSolverType = OdePack (Bdf, ChordWithDiagonalJacobian, UseNonNegative 1.0e-12)
            derivative = d.derivativeCalculator
        }

type OdeTestProgressData =
    {
        x : int
    }


type OdeTests (output : ITestOutputHelper) =
    let writeLine s = output.WriteLine s
    let nullString : string = null
    let errInvTolerance = 1.0e-3
    let outputFolder = @"C:\EeInf"

    /// Treat all values of u less than this as zero.
    let correctionValue = 1.0e-12

    let op_default : OdeTestData =
        {
            startTime = 0.0m |> EvolutionTime
            endTime = 10.0m |> EvolutionTime
            stepSize = 1.0e-3
            absoluteTolerance = AbsoluteTolerance.defaultValue
            odeSolverType = OdePack (Bdf, ChordWithDiagonalJacobian, UseNonNegative correctionValue)
        }

    let outputParams =
        {
            noOfOutputPoints = 4_000
            noOfProgressPoints = 100
            noOfResultDetailedPoints = Some 20
        }

    let op_10K = { op_default with endTime = 10_000.0m |> EvolutionTime }
    let op_50K = { op_default with endTime = 50_000.0m |> EvolutionTime }
    let op_65K = { op_default with endTime = 65_000.0m |> EvolutionTime }
    let op_100K = { op_default with endTime = 100_000.0m |> EvolutionTime }
    let op_200K = { op_default with endTime = 200_000.0m |> EvolutionTime }
    let op_1M = { op_default with endTime = 1_000_000.0m |> EvolutionTime }


    let outputMatrix (d : Domain2D) (m : Matrix<double>) =
        "000," + String.Join(",", d.infDomain.points.value)
        |> writeLine

        m.value
        |> Array.mapi (fun i e -> $"{d.eeDomain.points.value[i]}," + String.Join(",", e))
        |> Array.map (fun e -> (writeLine $"{e}"))
        |> ignore

        writeLine $"{Nl}===================================================================================={Nl}{Nl}"


    // let outputEeInfModelData (data : EeInfDiffModelParams) (odeParams : OdeParams) =
    //     writeLine $"data:{Nl}{(toOutputString data)}{Nl}"
    //     writeLine $"odeParams:{Nl}{(toOutputString odeParams)}"
    //
    //
    // let outputResult md d (v : SubstanceData) r =
    //     let sd = calculateDiffStat md v
    //
    //     let s = $"t: {d.t}, call count: {d.progressData.callCount}, " +
    //             $"progress: {d.progressData.progress}, " +
    //             $"inv: {sd.invariant}, total: {sd.total}, " +
    //             $"mean: {(sd.eeStatData.mean, sd.infStatData.mean)}, stdDev: {(sd.eeStatData.stdDev, sd.infStatData.stdDev)}.{Nl}"
    //
    //     writeLine s
    //
    //     if r
    //     then
    //         writeLine "u data:"
    //         v.protocell.toMatrix() |> outputMatrix md.kernelData.domain2D


    let outputKa (md : EeInfDiffModel) =
        writeLine "ka:"
        md.kernelData.ka.value |> outputMatrix md.kernelData.domain2D


    let outputGamma (md : EeInfDiffModel) =
        writeLine "gamma:"
        md.gamma.value |> outputMatrix md.kernelData.domain2D


    let outputChart b (cd : ChartDiffData) =
        if b then
            let f e =
                $"{e.tChart},{e.statData.eeStatData.mean},{e.statData.eeStatData.stdDev}," +
                $"{e.statData.infStatData.mean},{e.statData.infStatData.stdDev}," +
                $"{e.statData.invariant},{e.statData.total}," +
                $"{e.statData.food},{e.statData.waste}"

            let header = "t,eeMean,eeStdDev,infMean,infStdDev,invariant,total,food,waste"

            writeLine $"{Nl}{Nl}Chart data:"
            List.append [ header ] (cd.allChartData |> List.map f |> List.rev)
            |> List.map writeLine
            |> ignore

        let plotter = EeInfPlotter(cd)
        plotter.eeChart()
        plotter.uChart()


    let outputWolframData (odeParams : OdeTestData) name (d : OdeResultData) =
        let wolframFileName = $@"{outputFolder}\{name}.m"

        let wolframData = d.toWolframData odeParams
        File.WriteAllText(wolframFileName, wolframData)


    let nSolveParam p odeParams = failwith "nSolveParam is not implemented yet."
        // let md = EeInfDiffModel.create p
        // let i = md.diffInitialValues
        // // let f t v = outputResult md t v
        // let f t v = ()
        // let v x = LinearData<SubstanceType, double>.create i.value.dataInfo x |> SubstanceData
        //
        // let n =
        //     {
        //         odeParams = odeParams
        //         runQueueId = Guid() |> RunQueueId
        //         initialValues = i.value.data
        //         derivative = md.derivativeCalculator f i.value.dataInfo
        //         callBackInfo =
        //             {
        //                 checkFreq = TimeSpan.MaxValue
        //                 progressCallBack = ProgressCallBack (fun _ _ -> ())
        //                 chartCallBack = ChartCallBack (fun _ _ -> ())
        //                 chartDetailedCallBack = ChartDetailedCallBack (fun _ _ -> ())
        //                 checkCancellation = CheckCancellation (fun _ -> None)
        //             }
        //
        //         started = DateTime.Now
        //         logger =
        //             {
        //                 logCrit = fun e -> writeLine $"CRIT: {DateTime.Now}, {e}."
        //                 logError = fun e -> writeLine $"ERROR: {DateTime.Now}, {e}."
        //                 logWarn = fun e -> writeLine $"WARN: {DateTime.Now}, {e}."
        //                 logInfo = fun e -> writeLine $"INFO: {DateTime.Now}, {e}."
        //                 logDebug = fun e -> writeLine $"DEBUG: {DateTime.Now}, {e}."
        //             }
        //     }
        //
        // n, v


    // let callBack cr outputResult c (d : CallBackData) =
    //     match c with
    //     | RegularCallBack ->
    //         outputResult d
    //         { cr with progressCallBackCount = cr.progressCallBackCount + 1 }
    //     | FinalCallBack ct ->
    //         match ct with
    //         | CompletedCalculation ->
    //             outputResult d
    //             writeLine "CompletedCalculation."
    //             { cr with completedCallBackCount = cr.completedCallBackCount + 1 }
    //         | CancelledCalculation c ->
    //             writeLine $"CancelledCalculation: {c}."
    //
    //             match c with
    //             | CancelWithResults _  -> { cr with cancelledCallBackCount = cr.cancelledCallBackCount + 1 }
    //             | AbortCalculation _ -> { cr with abortedCallBackCount = cr.abortedCallBackCount + 1 }


    let chartCallBack cr _ _ = { cr with chartCallBackCount = cr.chartCallBackCount + 1 }


    let chartDetailedCallBack cr _ _ =
        { cr with chartCallBackCount = cr.chartCallBackCount + 1; chartDetailedCallBackCount = cr.chartDetailedCallBackCount + 1 }


    // let calLBackInfo n callBack charCallBack chartDetailedCallBack checkCancellation =
    //     {
    //         checkFreq = TimeSpan.FromMilliseconds(10.0)
    //         progressCallBack = ProgressCallBack callBack
    //         chartCallBack = ChartCallBack charCallBack
    //         chartDetailedCallBack = ChartDetailedCallBack chartDetailedCallBack
    //         checkCancellation = CheckCancellation checkCancellation
    //     }


    let odePackRun (p : EeInfDiffModelParams) cc odeParams = failwith "odePackRun is not implemented yet"
        // let mutable cr = CallBackResults.defaultValue
        // outputEeInfModelData p odeParams
        //
        // let md = EeInfDiffModel.create p
        // let n, getData = nSolveParam p odeParams
        // let outputResult b (d : CallBackData) = outputResult md d (getData d.x) b
        //
        // let callBack c d = cr <- callBack cr (outputResult false) c d
        //
        // let chartInitData =
        //     {
        //         baseData =
        //             {
        //                 resultId = RunQueueId.getNewId()
        //                 modelParams = md.diffModelParams.eeInfModelParams
        //                 domain2D = md.kernelData.domain2D
        //             }
        //         y0 = 0.0M
        //         tEnd = 0.0M
        //     }
        //
        // let chartDataUpdater = AsyncChartDiffDataUpdater(ChartDiffDataUpdater(), chartInitData)
        //
        // let getChartSliceData c b (d : CallBackData) : ChartSliceDiffData =
        //     let g() = getData d.x
        //
        //     let substanceData =
        //         match c with
        //         | RegularCallBack ->
        //             match b with
        //                 | false -> None
        //                 | true -> g() |> Some
        //         | FinalCallBack cct ->
        //             match cct with
        //             | CompletedCalculation -> g() |> Some
        //             | CancelledCalculation ct ->
        //                 match ct with
        //                 | AbortCalculation _ -> None
        //                 | CancelWithResults _ -> g() |> Some
        //     {
        //         tChart = d.t
        //         progressChart = d.progressData
        //         statData = calculateDiffStat md (getData d.x)
        //         substanceData = substanceData
        //     }
        //
        // let chartCallBack c d =
        //     // writeLine $"chartCallBack: t = {d.t}, progress = {d.progressData.progress}."
        //     getChartSliceData c false d |> chartDataUpdater.addContent
        //     cr <- chartCallBack cr c d
        //
        // let chartDetailedCallBack c d =
        //     // writeLine $"chartDetailedCallBack: t = {d.t}, progress = {d.progressData.progress}."
        //     getChartSliceData c true d |> chartDataUpdater.addContent
        //     cr <- chartDetailedCallBack cr c d
        //
        // let c = calLBackInfo n callBack chartCallBack chartDetailedCallBack cc
        //
        // let nSolveParam = { n with callBackInfo = c }
        // let invStart = getData nSolveParam.initialValues |> md.invariant
        // // outputKa md
        // // outputGamma md
        // // outputResult false { progressData = ProgressData.defaultValue; t = nSolveParam.odeParams.startTime; x = nSolveParam.initialValues }
        //
        // let result = nSolve nSolveParam
        // // outputResult true result
        // let chartData = chartDataUpdater.getContent()
        // chartData |> outputChart false
        //
        // let odeResult =
        //     {
        //         modelData = md
        //         callBackResults = cr
        //         result = result
        //         getData = getData
        //         invStart = invStart
        //         chartData = chartData
        //     }
        //
        // odeResult


    let odePackShouldRun (p : EeInfDiffModelParams) (odeParams : OdeTestData) shift name =
        let eeInfDiffModelParams = { p.named name with diffInitParams = p.diffInitParams.shifted shift }
        let r = odePackRun eeInfDiffModelParams (fun _ -> None) odeParams

        outputWolframData odeParams name r

        let v = r.getData r.x
        let inv_tEnd = r.modelData.invariant v
        let diff = (inv_tEnd - r.invStart) / r.invStart

        use _ = new AssertionScope()
        // r.result.Should().NotBeNull(nullString) |> ignore
        diff.Should().BeApproximately(0.0, errInvTolerance, nullString) |> ignore

    /// All flat - to study "diffusion".
    /// DomainIntervals 100
    /// k0 = 0.1, ka - identity
    /// eps0 = 0.01
    /// global asymmetry factor = 0 - no asymmetry
    let mp_d100k1e01a0 = EeInfDiffModelParams.defaultValue |> EeInfDiffModelParams.withDomainIntervals (DomainIntervals 100) |> EeInfDiffModelParams.withK0 K0.defaultValue
    let mp_d200k1e01a0 = EeInfDiffModelParams.defaultValue |> EeInfDiffModelParams.withDomainIntervals (DomainIntervals 200) |> EeInfDiffModelParams.withK0 K0.defaultValue

    /// eps0 = 0.005
    let mp_d200k1e005a0 = mp_d200k1e01a0 |> EeInfDiffModelParams.withEps0 Eps0.defaultNarrowValue


    /// DomainIntervals 100
    /// k0 = 0.1, ka - quadratic
    /// eps0 = 0.01
    /// gamma0 = 0.01
    /// global asymmetry factor = -0.01
    let mp_d100k1e01g01 = EeInfDiffModelParams.defaultNonLinearValue |> EeInfDiffModelParams.withDomainIntervals (DomainIntervals 100)

    /// DomainIntervals 200
    let mp_d200k1e01g01 = EeInfDiffModelParams.defaultNonLinearValue |> EeInfDiffModelParams.withDomainIntervals (DomainIntervals 200)


    /// eps0 = 0.02
    let mp_d100k1e02g01 = mp_d100k1e01g01 |> EeInfDiffModelParams.withEps0 Eps0.defaultWideValue
    let mp_d200k1e02g01 = mp_d200k1e01g01 |> EeInfDiffModelParams.withEps0 Eps0.defaultWideValue


    /// eps0 = 0.005
    let mp_d100k1e005g01 = mp_d100k1e01g01 |> EeInfDiffModelParams.withEps0 Eps0.defaultNarrowValue
    let mp_d200k1e005g01 = mp_d200k1e01g01 |> EeInfDiffModelParams.withEps0 Eps0.defaultNarrowValue


    /// k0 = 0.01, ka - quadratic
    let mp_d100k01e01g01 = mp_d100k1e01g01 |> EeInfDiffModelParams.withK0 K0.defaultSmallValue
    let mp_d200k01e01g01 = mp_d200k1e01g01 |> EeInfDiffModelParams.withK0 K0.defaultSmallValue

    /// k0 = 0.01, ka - quadratic
    /// eps0 = 0.005
    let mp_d200k01e005g01 = mp_d200k1e005g01 |> EeInfDiffModelParams.withK0 K0.defaultSmallValue

    /// k0 = 0.001, ka - quadratic
    /// eps0 = 0.005
    let mp_d200k001e005g01 = mp_d200k1e005g01 |> EeInfDiffModelParams.withK0 (K0 0.001)

    /// k0 = 0.0001, ka - quadratic
    /// eps0 = 0.005
    let mp_d200k0001e005g01 = mp_d200k1e005g01 |> EeInfDiffModelParams.withK0 (K0 0.0001)


    /// k0 = 0.001, ka - quadratic
    let mp_d100k001e01g01 = mp_d100k1e01g01 |> EeInfDiffModelParams.withK0 K0.defaultVerySmallValue
    let mp_d200k001e01g01 = mp_d200k1e01g01 |> EeInfDiffModelParams.withK0 K0.defaultVerySmallValue


    member private _.getCallerName([<CallerMemberName; Optional; DefaultParameterValue("")>] ?memberName: string) =
        let now = DateTime.Now
        let memberName = defaultArg memberName ""
        $"{memberName}__{now:yyyyMMdd_HHmm}"

    // ===== Flat - to study diffusion ===============

    [<Fact>]
    member t.odePack_ShouldRun () : unit = odePackShouldRun mp_d100k1e01a0 op_default 0 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01a0_50K () : unit = odePackShouldRun mp_d200k1e01a0 op_50K 0 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01a0_100K () : unit = odePackShouldRun mp_d200k1e01a0 op_100K 0 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005a0_50K () : unit = odePackShouldRun mp_d200k1e005a0 op_50K 0 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005a0_100K () : unit = odePackShouldRun mp_d200k1e005a0 op_100K 0 (t.getCallerName())

    // ===== 10K ===============

    [<Fact>]
    member t.d200k01e01g01_10K () : unit = odePackShouldRun mp_d200k01e01g01 op_10K 1 (t.getCallerName())

    [<Fact>]
    member t.d200k01e01g01_10K_s2 () : unit = odePackShouldRun mp_d200k01e01g01 op_10K 2 (t.getCallerName())

    [<Fact>]
    member t.d200k01e01g01_10K_s5 () : unit = odePackShouldRun mp_d200k01e01g01 op_10K 5 (t.getCallerName())

    // ===== 50K ===============

    [<Fact>]
    member t.d100k1e01g01_50K () : unit = odePackShouldRun mp_d100k1e01g01 op_50K 1 (t.getCallerName())

    [<Fact>]
    member t.d100k1e02g01_50K () : unit = odePackShouldRun mp_d100k1e02g01 op_50K 1 (t.getCallerName())

    [<Fact>]
    member t.d100k1e005g01_50K () : unit = odePackShouldRun mp_d100k1e005g01 op_50K 1 (t.getCallerName())

    [<Fact>]
    member t.d100k01e01g01_50K () : unit = odePackShouldRun mp_d100k01e01g01 op_50K 1 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01_50K () : unit = odePackShouldRun mp_d200k1e01g01 op_50K 1 (t.getCallerName())

    [<Fact>]
    member t.d200k1e02g01_50K () : unit = odePackShouldRun mp_d200k1e02g01 op_50K 1 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01_50K () : unit = odePackShouldRun mp_d200k1e005g01 op_50K 1 (t.getCallerName())

    [<Fact>]
    member t.d200k01e01g01_50K () : unit = odePackShouldRun mp_d200k01e01g01 op_50K 1 (t.getCallerName())

    [<Fact>]
    member t.d200k01e01g01_50K_s2 () : unit = odePackShouldRun mp_d200k01e01g01 op_50K 2 (t.getCallerName())

    [<Fact>]
    member t.d200k01e01g01_50K_s5 () : unit = odePackShouldRun mp_d200k01e01g01 op_50K 5 (t.getCallerName())

    [<Fact>]
    member t.d200k001e01g01_50K_s2 () : unit = odePackShouldRun mp_d200k001e01g01 op_50K 2 (t.getCallerName())

    [<Fact>]
    member t.d200k001e01g01_50K_s5 () : unit = odePackShouldRun mp_d200k001e01g01 op_50K 5 (t.getCallerName())

    [<Fact>]
    member t.d200k01e005g01_50K_s5 () : unit = odePackShouldRun mp_d200k01e005g01 op_50K 5 (t.getCallerName())

    [<Fact>]
    member t.d200k001e005g01_50K_s5 () : unit = odePackShouldRun mp_d200k001e005g01 op_50K 5 (t.getCallerName())

    [<Fact>]
    member t.d200k0001e005g01_50K_s5 () : unit = odePackShouldRun mp_d200k0001e005g01 op_50K 5 (t.getCallerName())

    // ===== 100K ===============

    [<Fact>]
    member t.d100k1e01g01_100K () : unit = odePackShouldRun mp_d100k1e01g01 op_100K 1 (t.getCallerName())

    [<Fact>]
    member t.d100k1e02g01_100K () : unit = odePackShouldRun mp_d100k1e02g01 op_100K 1 (t.getCallerName())

    [<Fact>]
    member t.d100k1e005g01_100K () : unit = odePackShouldRun mp_d100k1e005g01 op_100K 1 (t.getCallerName())

    [<Fact>]
    member t.d100k01e01g01_100K () : unit = odePackShouldRun mp_d100k01e01g01 op_100K 1 (t.getCallerName())

    [<Fact>]
    member t.d200k1e01g01_100K () : unit = odePackShouldRun mp_d200k1e01g01 op_100K 1 (t.getCallerName())

    [<Fact>]
    member t.d200k1e02g01_100K () : unit = odePackShouldRun mp_d200k1e02g01 op_100K 1 (t.getCallerName())

    [<Fact>]
    member t.d200k1e005g01_100K () : unit = odePackShouldRun mp_d200k1e005g01 op_100K 1 (t.getCallerName())

    [<Fact>]
    member t.d200k01e01g01_100K () : unit = odePackShouldRun mp_d200k01e01g01 op_100K 1 (t.getCallerName())

    [<Fact>]
    member t.d200k01e005g01_100K_s5 () : unit = odePackShouldRun mp_d200k01e005g01 op_100K 5 (t.getCallerName())

    [<Fact>]
    member t.d200k01e005g01_200K_s5 () : unit = odePackShouldRun mp_d200k01e005g01 op_200K 5 (t.getCallerName())

    [<Fact>]
    member t.d200k01e005g01_200K_s0 () : unit = odePackShouldRun mp_d200k01e005g01 op_200K 0 (t.getCallerName())

    [<Fact>]
    member t.d200k01e005g01_1M_s5 () : unit = odePackShouldRun mp_d200k01e005g01 op_1M 5 (t.getCallerName())

    [<Fact>]
    member t.d200k01e005g01_1M_s0 () : unit = odePackShouldRun mp_d200k01e005g01 op_1M 0 (t.getCallerName())

    [<Fact>]
    member t.d200k001e005g01_100K_s5 () : unit = odePackShouldRun mp_d200k001e005g01 op_100K 5 (t.getCallerName())

    [<Fact>]
    member t.d200k001e005g01_200K_s5 () : unit = odePackShouldRun mp_d200k001e005g01 op_200K 5 (t.getCallerName())

    [<Fact>]
    member t.d200k001e005g01_200K_s0 () : unit = odePackShouldRun mp_d200k001e005g01 op_200K 0 (t.getCallerName())

    [<Fact>]
    member t.d200k001e005g01_1M_s5 () : unit = odePackShouldRun mp_d200k001e005g01 op_1M 5 (t.getCallerName())

    [<Fact>]
    member t.d200k001e005g01_1M_s0 () : unit = odePackShouldRun mp_d200k001e005g01 op_1M 0 (t.getCallerName())

    [<Fact>]
    member t.d200k0001e005g01_100K_s5 () : unit = odePackShouldRun mp_d200k0001e005g01 op_100K 5 (t.getCallerName())

    [<Fact>]
    member t.odePack_ShouldCallBack () : unit =
        let name = t.getCallerName()
        let r = odePackRun (EeInfDiffModelParams.defaultValue.named name) (fun _ -> None) op_default

        use _ = new AssertionScope()
        // r.result.Should().NotBeNull(nullString) |> ignore
        r.callBackResults.progressCallBackCount.Should().BeGreaterThan(0, nullString) |> ignore
        r.callBackResults.completedCallBackCount.Should().BeGreaterThan(0, nullString) |> ignore
        r.callBackResults.cancelledCallBackCount.Should().Be(0, nullString) |> ignore
        r.callBackResults.chartCallBackCount.Should().BeGreaterThan(0, nullString) |> ignore

    [<Fact>]
    member t.odePack_ShouldCancel () : unit = failwith "odePack_ShouldCancel is not implemented yet."
        // let mutable cr = CallBackResults.defaultValue
        //
        // let md = EeInfDiffModelParams.defaultValue.named (t.getCallerName())
        // let model = EeInfDiffModel.create md
        // //let r = odePackRun md (fun _ -> None) defaultOdeParams
        //
        // let n, getData = nSolveParam md op_default
        // let outputResult b d = outputResult model d (getData d.x) b
        //
        // let callBack c d = cr <- callBack cr (outputResult false) c d
        // let chartCallBack c d = cr <- chartCallBack cr c d
        // let chartDetailedCallBack c d = cr <- chartDetailedCallBack cr c d
        // let checkCancellation _ = CancelWithResults None |> Some
        // let c = calLBackInfo n callBack chartCallBack chartDetailedCallBack checkCancellation
        //
        // let nSolveParam = { n with callBackInfo = c }
        //
        // use _ = new AssertionScope()
        // FluentActions.Invoking(fun () -> nSolve nSolveParam |> ignore).Should().Throw<ComputationAbortedException>(nullString) |> ignore
        // cr.progressCallBackCount.Should().BeGreaterThan(0, nullString) |> ignore
        // cr.completedCallBackCount.Should().Be(0, nullString) |> ignore
        // cr.cancelledCallBackCount.Should().Be(1, nullString) |> ignore
        // cr.abortedCallBackCount.Should().Be(0, nullString) |> ignore

    [<Fact>]
    member _.odePack_ShouldAbort () : unit = failwith "odePack_ShouldAbort is not implemented yet."
        // let mutable cr = CallBackResults.defaultValue
        //
        // let md = EeInfDiffModelParams.defaultValue
        // let model = EeInfDiffModel.create md
        //
        // let n, getData = nSolveParam md op_default
        // let outputResult b d = outputResult model d (getData d.x) b
        //
        // let callBack c d = cr <- callBack cr (outputResult false) c d
        // let chartCallBack c d = cr <- chartCallBack cr c d
        // let chartDetailedCallBack c d = cr <- chartDetailedCallBack cr c d
        // let checkCancellation _ = AbortCalculation None |> Some
        // let c = calLBackInfo n callBack chartCallBack chartDetailedCallBack checkCancellation
        //
        // let nSolveParam = { n with callBackInfo = c }
        //
        // use _ = new AssertionScope()
        // FluentActions.Invoking(fun () -> nSolve nSolveParam |> ignore).Should().Throw<ComputationAbortedException>(nullString) |> ignore
        // cr.progressCallBackCount.Should().BeGreaterThan(0, nullString) |> ignore
        // cr.completedCallBackCount.Should().Be(0, nullString) |> ignore
        // cr.cancelledCallBackCount.Should().Be(0, nullString) |> ignore
        // cr.abortedCallBackCount.Should().Be(1, nullString) |> ignore

    [<Fact>]
    member _.integrate_ShouldWork () : unit =

        let integrate2D (grid: double[][]) dx dy =
            let len1 = grid.Length - 1
            let len2 = grid[0].Length - 1
            let sum = grid |> Array.map (fun e -> e |> Array.sum) |> Array.sum
            let edgeSum1 = grid[0] |> Array.sum
            let edgeSum2 = grid[len1] |> Array.sum
            let edgeSum3 = grid |> Array.mapi (fun i _ -> grid[i][0]) |> Array.sum
            let edgeSum4 = grid |> Array.mapi (fun i _ -> grid[i][len2]) |> Array.sum
            let cornerSum = grid[0][0] + grid[len1][0] + grid[0][len2] + grid[len1][len2]
            let retVal = dx * dy * (4.0 * sum - 2.0 * (edgeSum1 + edgeSum2 + edgeSum3 + edgeSum4) + cornerSum) / 4.0
            retVal

        let xSteps = 101
        let ySteps = 101
        let dx = 1.0 / (double (xSteps - 1))
        let dy = 1.0 / (double (ySteps - 1))

        let grid = Array.init xSteps (fun x -> Array.init ySteps (fun y -> float x * dx * float y * dy))
        let result = integrate2D grid dx dy
        result.Should().BeApproximately(0.25, 0.001, nullString) |> ignore

    [<Fact>]
    member t.chart_ShouldWork() : unit =
        let xData = [0. .. 10.]
        let yData = [0. .. 10.]
        let description = toDescription "Title" "description"
        let chart = Chart.Point(xData, yData, UseDefaults = false)
        let htmlString = toEmbeddedHtmlWithDescription description chart
        let tempFilePath = $@"{outputFolder}\{t.getCallerName()}.html"
        File.WriteAllText(tempFilePath, htmlString)
