namespace FredholmSolver

open System.Diagnostics
open System.IO
open System
open FredholmSolver.Primitives
open FredholmSolver.Kernel
open FredholmSolver.EeInfIntModel2
open FredholmSolver.EeInfChartData
open Softellect.DistributedProcessing.Primitives.Common
open Softellect.DistributedProcessing.Proxy.ModelGenerator
open Softellect.DistributedProcessing.SolverRunner.Primitives
open Softellect.Sys.Primitives
open Softellect.Sys.Core
open Softellect.Analytics.Wolfram
open Softellect.DistributedProcessing.ModelGenerator.Program
open Softellect.Math.Primitives
open Softellect.Math.Models

module PoissonSolver2 =

    let poissonSolverId = "0275536F-53F8-4CEF-99AE-CFCC31253557" |> Guid.Parse |> SolverId
    let poissonSolverName = "Poisson2" |> SolverName
    let getNamePrefix name = $"{name}__"

    type EeInfIntModelParams = EeInfIntModel.EeInfIntModelParams
    type PoissonEvolutionParam = PoissonSolver.PoissonEvolutionParam


    // type PoissonEvolutionParam =
    //     {
    //         noOfEpochs : NoOfEpochs
    //         noOfCharts : int option
    //         maxChartPoints : int
    //         noOfFrames : int option
    //         duration : int // Clip duration in seconds.
    //         name : string
    //         outputFolder : FolderName
    //         dataFolder : FolderName
    //         odePackChartSupportFolder : FolderName
    //     }


    /// That's 'I in the type signature.
    type PoissonInitialData =
        {
            intModelParams : EeInfIntModelParams
            evolutionParam : PoissonEvolutionParam
        }

        member p.fullName = p.evolutionParam.name


    type PoissonParam =
        {
            runQueueId : RunQueueId
            initialData : PoissonInitialData
        }

        static member defaultValue mp n name =
            {
                runQueueId = RunQueueId.getNewId()
                initialData =
                    {
                        intModelParams = mp
                        evolutionParam =
                            {
                                noOfEpochs = n
                                noOfCharts = Some 20
                                maxChartPoints = 100_000
                                noOfFrames = Some 2_000
                                duration = 50
                                name = name
                                outputFolder = FolderName @"C:\EeInf"
                                dataFolder = FolderName @"C:\EeInf\Data"
                                odePackChartSupportFolder = FolderName @"C:\\GitHub\\CoreClm\\Math\\odePackChartSupport.m" // Need \\ for Wolfram.
                            }
                    }
            }

        member p.modelString =
            let a = p.initialData.intModelParams.modelString
            let b = p.initialData.evolutionParam.noOfEpochs.value |> int64 |> toModelStringInt64 0L |> Option.defaultValue EmptyString
            $"{a}_{b}"

        member p.fullName = p.initialData.fullName


    let createModel (mp : EeInfIntModelParams) name =
        let md = mp.named name
        let model = EeInfIntModel.create md
        model


    let getChartInitData (model : EeInfIntModel) noOfEpochs =
        {
            baseData =
                {
                    resultId = RunQueueId.getNewId()
                    modelParams = model.intModelParams.eeInfModelParams
                    domain2D = model.domain2D
                }
            totalMolecules = model.intModelParams.intInitParams.totalMolecules
            noOfEpochs = noOfEpochs
        }


    let getChartSliceData (model : EeInfIntModel2.EeInfIntModel) (NoOfEpochs noOfEpochs) chartMod e i : ChartSliceIntData =
        {
            epochNumber = i
            progress = (decimal i) / (decimal noOfEpochs)
            statData  = calculateIntStat2 model e
            substanceData  =
                match chartMod with
                // | Some v -> if i % v = 0 then Some e else None
                | Some v -> None
                | None -> None
        }

    let outputProgress (sw : Stopwatch) noOfEpochs progressFreq i =
        if i % progressFreq = 0 || i <= 10 then
            let t = double sw.ElapsedMilliseconds
            let dt = t / (double (i + 1))
            let estCompl = DateTime.Now.AddMilliseconds(dt * (double (noOfEpochs - i - 1))).ToString("yyyy-MM-dd HH:mm:ss")
            printfn $"Completed {i} of {noOfEpochs} steps in {(t / 60_000.0):N2} minutes. Estimated completion: {estCompl}."


    let toWolframData (model : EeInfIntModel) (p : PoissonEvolutionParam) (substanceData : SubstanceData) (chartData : ChartIntData) =
        let a = $"""Get["{p.odePackChartSupportFolder}"];{Nl}{Nl}"""
        let b = $"""plotAll[1];{Nl}"""
        let d = model.intModelParams |> toOutputString |> toWolframNotation
        let descr = $"descr ={Nl}\"{d}{Nl}noOfEpochs = {p.noOfEpochs}\";{Nl}{Nl}"
        let gamma0 = model.intModelParams.eeInfModelParams.gammaFuncValue.gamma0.value
        let k0 = model.intModelParams.eeInfModelParams.kernelParams.kaFuncValue.k0.value
        let totalMolecules = model.intModelParams.intInitParams.totalMolecules.value
        let norm = 100.0 / (double totalMolecules) // Use values in %.

        let eta = model.domain2D.eeDomain.points.value
        let zeta = model.domain2D.infDomain.points.value
        let etaData = $"etaData = {(toWolframNotation eta)};{Nl}{Nl}"
        let zetaData = $"zetaData = {(toWolframNotation zeta)};{Nl}{Nl}"
        let k0Data = $"k0 = {(toWolframNotation k0)};{Nl}{Nl}"
        let gamma0Data = $"gamma0 = {(toWolframNotation gamma0)};{Nl}{Nl}"

        let u = (substanceData.protocell.value.convert (fun e -> norm * (double e)))
        let uData = $"uData = {(toWolframNotation u)};{Nl}{Nl}"

        // Need to rescale ka back.
        let n = model.intModelParams.eeInfModelParams.numberOfMolecules.value
        let kMult = pown (double totalMolecules) n

        // let ka = model.kernelData.ka.value.value |> Array.map (fun a -> a |> Array.map (fun b -> b * kMult / k0))
        let ka = 0

        let kaData = $"ka = {(toWolframNotation ka)};{Nl}{Nl}"

        // let gamma = model.gamma.value.value |> Array.map (fun a -> a |> Array.map (fun b -> b / gamma0))
        let gamma = 0
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


    let outputFrameData (model : EeInfIntModel) (p : PoissonParam) (substanceData : SubstanceData) i =
        let name = model.intModelParams.eeInfModelParams.name
        let wolframFileName = $@"{p.initialData.evolutionParam.dataFolder}\{(getNamePrefix name)}{i:D8}.m"
        let totalMolecules = model.intModelParams.intInitParams.totalMolecules.value
        let norm = 100.0 / (double totalMolecules) // Use values in %.
        let eta = model.domain2D.eeDomain.points.value
        let zeta = model.domain2D.infDomain.points.value
        let u = (substanceData.protocell.value.convert (fun e -> norm * (double e)))
        let xyz = eta |> Array.mapi (fun i a-> zeta |> Array.mapi (fun j b -> [ a; b; u.map.Value |> Map.tryFind { i0 = i; i1 = j } |> Option.defaultValue 0.0 ])) |> Array.concat
        let wolframData = $"{(toWolframNotation xyz)}{Nl}{Nl}"
        File.WriteAllText(wolframFileName, wolframData)


    let toWolframAnimation name duration =
        let a = $"""Get["C:\\GitHub\\CoreClm\\Math\\odePackChartSupport.m"];{Nl}"""
        let b = $"""createAnimation["{(getNamePrefix name)}", Large, {duration}];{Nl}"""
        $"{a}{b}"


    let outputChart writeLine (chartData : ChartIntData) =
        writeLine "epochNumber,food,waste,total,invariant,eeMean,eeStdDev,infMean,infStdDev"

        chartData.allChartData
        |> List.rev
        |> List.map (fun e -> writeLine $"{e.epochNumber},{e.statData.food},{e.statData.waste},{e.statData.total},{e.statData.invariant},{e.statData.eeStatData.mean},{e.statData.eeStatData.stdDev},{e.statData.infStatData.mean},{e.statData.infStatData.stdDev}")
        |> ignore


    /// That's 'D in the type signature.
    type PoissonSolverData =
        {
            initialData : PoissonInitialData
            model : EeInfIntModel
            norm : double // Use values in %.
        }

        member p.getInitialData() = p.model.intInitialValues
        member p.fullName = p.initialData.evolutionParam.name


    /// That's 'P in the type signature.
    type PoissonProgressData =
        {
            x : int
        }

        static member defaultValue =
            {
                x = 0
            }


    /// That's 'C in the type signature.
    type PoissonChartData = ChartSliceIntData


    let createPoissonSolverData (i : PoissonInitialData) : PoissonSolverData =
        let model = createModel i.intModelParams i.fullName

        {
            initialData = i
            model = model
            norm = 100.0 / (double i.intModelParams.intInitParams.totalMolecules.value) // Use values in %.
        }


    let poissonModelGenerator systemProxy (i : PoissonInitialData) =
        let userProxy =
            {
                getInitialData = fun () -> i
                generateModelData = createPoissonSolverData

                getSolverInputParams = fun _ ->
                    {
                        startTime = EvolutionTime.defaultValue
                        endTime = i.evolutionParam.noOfEpochs.value |> decimal |> EvolutionTime
                    }

                getSolverOutputParams = fun _ ->
                    {
                        noOfOutputPoints = 2000
                        noOfProgressPoints = 100
                        noOfResultDetailedPoints = i.evolutionParam.noOfFrames
                    }
            }

        let result = generateModel<PoissonInitialData, PoissonSolverData> systemProxy poissonSolverId userProxy
        printfn $"result: '%A{result}'."
        result


    let poissonSolverRunner (p : PoissonSolverData) =
        let noOfEpochs = p.initialData.evolutionParam.noOfEpochs.value
        let psCount = p.initialData.intModelParams.eeInfModelParams.kernelParams.domainIntervals.value + 1

        let solve (_, x0) (tryCallBack : TryCallBack<SubstanceData>) =
            let evolve e i =
                let e1 = p.model.evolve e
                tryCallBack.invoke (i |> decimal |> EvolutionTime) e1
                e1

            let result = [|for i in 0..noOfEpochs -> i |] |> Array.fold evolve x0
            (noOfEpochs |> decimal |> EvolutionTime), result

        SolverRunner solve


    let runPoissonEvolution writeLine (p : PoissonParam) =
        let model = createModel p.initialData.intModelParams p.fullName
        let noOfEpochs = p.initialData.evolutionParam.noOfEpochs.value
        let progressFreq = noOfEpochs / 100
        let initialValue = model.intInitialValues
        let startInv = model.invariant initialValue
        let startStat = calculateIntStat2 model initialValue
        let psCount = p.initialData.intModelParams.eeInfModelParams.kernelParams.domainIntervals.value + 1
        let chartMod = p.initialData.evolutionParam.noOfCharts |> Option.bind (fun v -> noOfEpochs / v |> Some)
        let frameMod = p.initialData.evolutionParam.noOfFrames |> Option.bind (fun v ->  max (noOfEpochs / v) 1 |> Some)
        let chartFrequency = if noOfEpochs <= p.initialData.evolutionParam.maxChartPoints then 1 else noOfEpochs / p.initialData.evolutionParam.maxChartPoints

        let chartInitData = getChartInitData model p.initialData.evolutionParam.noOfEpochs
        let chartDataUpdater = AsyncChartIntDataUpdater(ChartIntDataUpdater(), chartInitData) :> IAsyncUpdater<ChartSliceIntData, ChartIntData>
        let getChartSliceData = getChartSliceData model (NoOfEpochs noOfEpochs) chartMod
        let outputFrameData = outputFrameData model p
        let outputChart = outputChart writeLine
        let sw = Stopwatch.StartNew()

        let evolve e i =
            let e1 = model.evolve e
            outputProgress sw noOfEpochs progressFreq i
            if i % chartFrequency = 0 then getChartSliceData e1 i |> chartDataUpdater.addContent

            match frameMod with
            | Some v -> if i % v = 0 then outputFrameData e1 i
            | None -> ()

            e1

        let result = [|for i in 0..noOfEpochs -> i |] |> Array.fold evolve initialValue
        let endInv = model.invariant result
        let endStat = calculateIntStat2 model result
        let chartData = chartDataUpdater.getContent()
        chartData |> outputChart

        let wolframFileName = $@"{p.initialData.evolutionParam.outputFolder}\{p.fullName}.m"
        let wolframData = toWolframData model p.initialData.evolutionParam result chartData
        File.WriteAllText(wolframFileName, wolframData)

        let wolframAnimationFileName = $@"{p.initialData.evolutionParam.outputFolder}\{getNamePrefix p.fullName}animation.m"
        let wolframAnimationData = toWolframAnimation p.fullName p.initialData.evolutionParam.duration
        File.WriteAllText(wolframAnimationFileName, wolframAnimationData)

        let elapsedTime = sw.Elapsed
        let formattedTime = $"%02d{elapsedTime.Days} %02d{elapsedTime.Hours}:%02d{elapsedTime.Minutes}:%02d{elapsedTime.Seconds}"
        writeLine $"Elapsed Time: {formattedTime}."

        writeLine $"noOfEpochs = {noOfEpochs}, noOfDomainPoints = {p.initialData.intModelParams.eeInfModelParams.kernelParams.domainIntervals}"
        writeLine $"startInv = {startInv}, endInv = {endInv}"
        writeLine $"start: food = {startStat.food}, waste = {startStat.waste}, u = {startStat.total}"
        writeLine $"start: ee mean = {startStat.eeStatData.mean}, ee stdDev = {startStat.eeStatData.stdDev}, inf mean = {startStat.infStatData.mean}, inf stdDev = {startStat.infStatData.stdDev}"
        writeLine $"end: food = {endStat.food}, waste = {endStat.waste}, u = {endStat.total}"
        writeLine $"end: ee mean = {endStat.eeStatData.mean}, ee stdDev = {endStat.eeStatData.stdDev}, inf mean = {endStat.infStatData.mean}, inf stdDev = {endStat.infStatData.stdDev}"

        // TODO kk:20231216 - Temporarily return this. Decide what to do with the return value. It is not really needed except for tests.
        startInv, endInv
