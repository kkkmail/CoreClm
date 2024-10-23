namespace FredholmSolver

open System.IO
open FredholmSolver.EeInfChartData
open Plotly.NET
open Primitives.ChartPrimitives
open Softellect.Sys.Primitives

module EeInfCharts =

    type EeInfPlotter(p : ChartDiffData) =
        let getFileName (ct : ChartType) =
            let name = p.initData.baseData.modelParams.name
                // match p.initData.baseData.modelParams.name with
                // | Some v -> v
                // | None -> $"{p.initData.baseData.resultId.value}"

            FileName $@"C:\EeInf\{name}__{ct.fileSuffix}.html"

        let noOfOutputPoints = p.allChartData.Length - 1
        let allChartData = p.allChartData |> List.rev |> Array.ofList
        let minMax = (0.0, float (getLastT p))
        let tIdx = [ for i in 0..noOfOutputPoints -> i ]
        let xAxisName = "t"

        let getDescription h =
            $"{p.initData.baseData.modelParams}"
            |> toDescription h

        // | PlotEeMu
        // | PlotEeStdDev
        // | PlotInfMu
        // | PlotInfStdDev
        // | PlotKa
        // | PlotU
        // | PlotGamma

        let getEeChart () =
            let fileName = getFileName ChartType.PlotEeMu
            let eeMuData = tIdx |> List.map (fun t -> (allChartData[t].tChart, allChartData[t].statData.eeStatData.mean))
            let eeStdDevData = tIdx |> List.map (fun t -> (allChartData[t].tChart, allChartData[t].statData.eeStatData.stdDev))

            let charts =
                [
                    Chart.Line(eeMuData, Name = "ee - mu")
                    Chart.Line(eeStdDevData, Name = "ee - stdDev")
                ]

            Chart.combine(charts)
            |> Chart.withXAxisStyle(xAxisName, MinMax = minMax)
            |> Chart.withTemplate ChartTemplates.light
            |> getChart fileName (getDescription "ee: mu, stdDev")

        let getUChart () =
            let fileName = getFileName ChartType.PlotU

            match p.allChartData |> List.tryHead |> Option.bind (fun e -> e.substanceData) with
            | Some d ->
                // Axes are swapped.
                let x = p.initData.baseData.domain2D.infDomain.points.value
                let y = p.initData.baseData.domain2D.eeDomain.points.value
                let u = d.protocell.toMatrix().value

                let surface =
                    Chart.Surface(
                        zData = u,
                        X = x,
                        Y = y)
                    |> getChart fileName (getDescription "U")

                Some surface
            | None -> None


        member _.eeChart() =
            let chart = getEeChart()
            File.WriteAllText(chart.fileName.value, chart.htmlContent)

        member _.uChart() =
            match getUChart() with
            | Some chart -> File.WriteAllText(chart.fileName.value, chart.htmlContent)
            | None -> ()
