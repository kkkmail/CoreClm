namespace FredholmSolver

open Primitives.GeneralData
open System
open GenericOdeSolver.Primitives
open GenericOdeSolver.Solver
open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives
open FredholmSolver.Primitives
open FredholmSolver.Kernel
open FredholmSolver.EeInfModel
open Primitives.SolverRunnerErrors
open Softellect.Sys.Core
open Softellect.Sys.Logging

module EeInfChartData =

    type StatData =
        {
            mean : double
            stdDev : double
        }


    type EeInfStatData =
        {
            eeStatData : StatData
            infStatData : StatData
            invariant : double
            total : double
            food : double
            waste : double
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



    type ChartInitData =
        {
            y0 : decimal
            tEnd : decimal
        }


    type ChartSliceData =
        {
            tChart : double
            progressChart : ProgressData
            statData : EeInfStatData
        }


    type ChartData =
        {
            startedOn : DateTime
            initData : ChartInitData
            allChartData : list<ChartSliceData>
        }

        static member create i =
            {
                startedOn = DateTime.Now
                initData = i
                allChartData = []
            }

        /// Last calculated value of tEnd.
        member cd.tLast =
            match cd.allChartData |> List.tryHead with
            | Some c -> c.tChart
            | None -> 0.0
            |> decimal

        member cd.progress =
            let tEnd = cd.initData.tEnd
            min (max (if tEnd > 0.0m then cd.tLast / tEnd else 0.0m) 0.0m) 1.0m


    type ChartDataUpdater () =
        interface IUpdater<ChartInitData, ChartSliceData, ChartData> with
            member _.init i = ChartData.create i
            member _.add a m = { m with allChartData = a :: m.allChartData }


    type AsyncChartDataUpdater = AsyncUpdater<ChartInitData, ChartSliceData, ChartData>


    let calculateStat (md : EeInfModel) (v : SubstanceData) =
        let n = md.modelParams.numberOfMolecules.value
        let u = v.protocell
        let total = md.kernel.domain2D.integrateValues u
        let inv = md.invariant v
        let mEe, mInf = md.kernel.domain2D.mean u
        let sEe, sInf = md.kernel.domain2D.stdDev u

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
