namespace Analytics

open System
open System.IO
open Microsoft.FSharp.Core
open ClmSys.ContGenPrimitives
open Clm.ModelParams
open Clm.ChartData
open FSharp.Plotly
open FSharp.Plotly.GenericChart
open FSharp.Plotly.ChartDescription
open ClmSys.GeneralErrors
open ClmSys.ClmErrors

module ChartExt =

    let toDescription h t =
        {
            Heading = h
            Text = t
        }


    type Chart with

        /// Show chart in browser
        static member ShowWithDescription1 (show : bool) (d : string) (ch:GenericChart) =
            let guid = Guid.NewGuid().ToString()
            let description = toDescription "" d
            let html = GenericChart.toEmbeddedHtmlWithDescription description ch
            let tempPath = Path.GetTempPath()
            let file = sprintf "%s.html" guid
            let path = Path.Combine(tempPath, file)
            File.WriteAllText(path, html)
            if show then System.Diagnostics.Process.Start(path) |> ignore


        /// Saves chart in a specified file name and shows it in the browser. The caller is responsible for full path / filename / extension.
        static member ShowFileWithDescription (show : bool) (fullFileName : string) (d : string) (ch:GenericChart) =
            let description = toDescription "" d
            let html = GenericChart.toEmbeddedHtmlWithDescription description ch
            File.WriteAllText(fullFileName, html)
            if show then System.Diagnostics.Process.Start(fullFileName) |> ignore


    type PlotDataInfo =
        {
            resultInfo : ResultInfo
            useTempFolder : bool
        }

        static member defaultValue =
            {
                resultInfo = ResultInfo.defautlValue
                useTempFolder = false
            }


    type ChartType
        with

        member ct.fileSuffix =
            match ct with
            | PlotAminoAcids -> "aa"
            | PlotEnantiomericExcess -> "ee"
            | PlotTotalSubst -> "ts"

        member private ct.getFileNameImpl (i : PlotDataInfo) (modelDataId : ModelDataId) (y0 : decimal) (tEnd : decimal) =
            let suff = ct.fileSuffix

            let fileName =
                [
                    modelDataId.value |> toModelName
                    i.resultInfo.separator
                    (int y0).ToString().PadLeft(3, '0')
                    (int tEnd).ToString().PadLeft(5, '0')
                    suff
                ]
                |> String.concat i.resultInfo.separator

            printfn "ChartType.getFileNameImpl: Creating folder: %A" i.resultInfo.resultLocation
            Directory.CreateDirectory(i.resultInfo.resultLocation) |> ignore
            Path.Combine(i.resultInfo.resultLocation, fileName + ".html")

        member ct.getFileName (i : PlotDataInfo, r : FullResultData) = ct.getFileNameImpl i r.resultData.modelDataId r.resultData.y0 r.resultData.tEnd
        member ct.getFileName (i : PlotDataInfo, d : ChartData) = ct.getFileNameImpl i d.initData.modelDataId d.initData.y0 d.initData.tEnd


    let showChart (i : PlotDataInfo) show fileName =
        match i.useTempFolder with
        | true -> Chart.ShowWithDescription1 show
        | false -> Chart.ShowFileWithDescription show fileName


    let getChart fileName d ch =
        {
            htmlContent = GenericChart.toEmbeddedHtmlWithDescription d ch
            fileName = fileName
        }


    let showHtmlChart chart =
        try
            File.WriteAllText(chart.fileName, chart.htmlContent)
            System.Diagnostics.Process.Start(chart.fileName) |> ignore
            Ok()
        with
        | e -> e |> GeneralFileExn |> FileErr |> Result.Error
