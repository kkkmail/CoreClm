namespace FredholmSolver

open FredholmSolver.EeInfIntModel
open System
open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives
open FredholmSolver.Kernel
open FredholmSolver.EeInfDiffModel
open Softellect.Sys.Core

module EeInfChartData =

    type StatData =
        {
            mean : double
            stdDev : double
        }


    type EeInfStatData<'T> =
        {
            eeStatData : StatData
            infStatData : StatData
            invariant : 'T
            total : 'T
            food : 'T
            waste : 'T
        }


    type ChartType =
        | PlotEeMu
        | PlotEeStdDev
        | PlotInfMu
        | PlotInfStdDev
        | PlotKa
        | PlotU
        | PlotGamma

        member ct.fileSuffix =
            match ct with
            | PlotEeMu -> "EeMu"
            | PlotEeStdDev -> "EeStdDev"
            | PlotInfMu -> "InfMu"
            | PlotInfStdDev -> "InfStdDev"
            | PlotKa -> "Ka"
            | PlotU -> "U"
            | PlotGamma -> "Gamma"

    type ChartInitBaseData =
        {
            resultId : RunQueueId
            modelParams : EeInfModelParams
            domain2D : Domain2D
        }


    type ChartInitDiffData =
        {
            baseData : ChartInitBaseData
            y0 : decimal
            tEnd : decimal
        }


    type ChartInitIntData =
        {
            baseData : ChartInitBaseData
            totalMolecules : int64
            noOfEpochs : int
        }


    type ChartSliceDiffData =
        {
            tChart : double
            progressChart : ProgressData
            statData : EeInfStatData<double>
            substanceData : SubstanceData option
        }


    type ChartSliceIntData =
        {
            epochNumber : int
            progress : decimal
            statData : EeInfStatData<int64>
            substanceData : SubstanceIntData option
        }


    type ChartData<'I, 'S> =
        {
            startedOn : DateTime
            initData : 'I
            allChartData : list<'S>
        }

        static member create i =
            {
                startedOn = DateTime.Now
                initData = i
                allChartData = []
            }

        // /// Last calculated value of tEnd.
        // member cd.tLast =
        //     match cd.allChartData |> List.tryHead with
        //     | Some c -> c.tChart
        //     | None -> 0.0
        //     |> decimal
        //
        // member cd.progress =
        //     let tEnd = cd.initData.tEnd
        //     min (max (if tEnd > 0.0m then cd.tLast / tEnd else 0.0m) 0.0m) 1.0m

    type ChartDiffData = ChartData<ChartInitDiffData, ChartSliceDiffData>
    type ChartIntData = ChartData<ChartInitIntData, ChartSliceIntData>


    let getLastT (cd : ChartDiffData) =
        match cd.allChartData |> List.tryHead with
        | Some c -> c.tChart
        | None -> 0.0
        |> decimal


    type ChartDataUpdater<'I, 'S> () =
        interface IUpdater<'I, 'S, ChartData<'I, 'S>> with
            member _.init i = ChartData<'I, 'S>.create i
            member _.add a m = { m with allChartData = a :: m.allChartData }


    type ChartDiffDataUpdater = ChartDataUpdater<ChartInitDiffData, ChartSliceDiffData>
    type ChartIntDataUpdater = ChartDataUpdater<ChartInitIntData, ChartSliceIntData>
    type AsyncChartDiffDataUpdater = AsyncUpdater<ChartInitDiffData, ChartSliceDiffData, ChartDiffData>
    type AsyncChartIntDataUpdater = AsyncUpdater<ChartInitIntData, ChartSliceIntData, ChartIntData>


    let calculateDiffStat (md : EeInfDiffModel) (v : SubstanceData) =
        let n = md.diffModelParams.eeInfModelParams.numberOfMolecules.value
        let u = v.protocell
        let total = md.kernelData.domain2D.integrateValues u
        let inv = md.invariant v
        let mEe, mInf = md.kernelData.domain2D.mean u
        let sEe, sInf = md.kernelData.domain2D.stdDev u

        {
            eeStatData =
                {
                    mean = mEe
                    stdDev = sEe
                }
            infStatData =
                {
                    mean = mInf
                    stdDev = sInf
                }
            invariant = inv
            total = (double n) * total
            food = v.food
            waste = (double n) * v.waste
        }


    let calculateIntStat (md : EeInfIntModel) (v : SubstanceIntData) =
        let n = int64 md.intModelParams.eeInfModelParams.numberOfMolecules.value
        let u = v.protocell.value
        let total = u.total()
        let inv = md.invariant v
        let mEe, mInf = md.kernelData.domain2D.mean u
        let sEe, sInf = md.kernelData.domain2D.stdDev u

        {
            eeStatData =
                {
                    mean = mEe
                    stdDev = sEe
                }
            infStatData =
                {
                    mean = mInf
                    stdDev = sInf
                }
            invariant = inv
            total = n * total
            food = v.food.value
            waste = n * v.waste.value
        }
