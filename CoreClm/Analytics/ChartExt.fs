﻿namespace Analytics

open System
open System.IO
open Microsoft.FSharp.Core
open ClmSys.ContGenPrimitives
open Clm.ModelParams
open Clm.ChartData
open ClmSys.GeneralErrors
open ClmSys.ClmErrors
open Primitives.ChartPrimitives
open Plotly.NET
open Giraffe.ViewEngine
open Softellect.Sys.Primitives

module ChartExt =

    type Chart with

        /// Show chart in browser
        static member ShowWithDescription1 (show : bool) (d : string) (ch:GenericChart) =
            let guid = Guid.NewGuid().ToString()
            let description = toDescription "" d
            let html = toEmbeddedHtmlWithDescription description ch
            let tempPath = Path.GetTempPath()
            let file = $"%s{guid}.html"
            let path = Path.Combine(tempPath, file)
            File.WriteAllText(path, html)
            if show then System.Diagnostics.Process.Start(path) |> ignore


        /// Saves chart in a specified file name and shows it in the browser. The caller is responsible for full path / filename / extension.
        static member ShowFileWithDescription (show : bool) (fullFileName : string) (d : string) (ch:GenericChart) =
            let description = toDescription "" d
            let html = toEmbeddedHtmlWithDescription description ch
            File.WriteAllText(fullFileName, html)
            if show then System.Diagnostics.Process.Start(fullFileName) |> ignore


    type PlotDataInfo =
        {
            resultInfo : ResultInfo
            useTempFolder : bool
        }

        static member defaultValue =
            {
                resultInfo = ResultInfo.defaultValue
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
            let suffix = ct.fileSuffix

            let fileName =
                [
                    modelDataId.value |> toModelName
                    i.resultInfo.separator
                    (int y0).ToString().PadLeft(3, '0')
                    (int tEnd).ToString().PadLeft(5, '0')
                    suffix
                ]
                |> String.concat i.resultInfo.separator

            printfn $"ChartType.getFileNameImpl: Creating folder: %A{i.resultInfo.resultLocation}"
            Directory.CreateDirectory(i.resultInfo.resultLocation) |> ignore
            Path.Combine(i.resultInfo.resultLocation, fileName + ".html")

        member ct.getFileName (i : PlotDataInfo, d : ChartData) =
            ct.getFileNameImpl i d.initData.modelDataId d.initData.y0 d.initData.tEnd
            |> FileName


    let showChart (i : PlotDataInfo) show fileName =
        match i.useTempFolder with
        | true -> Chart.ShowWithDescription1 show
        | false -> Chart.ShowFileWithDescription show fileName


    let showHtmlChart (chart : TextResult) =
        try
            File.WriteAllText(chart.fileName.value, chart.textContent)
            System.Diagnostics.Process.Start(chart.fileName.value) |> ignore
            Ok()
        with
        | e -> e |> GeneralFileExn |> FileErr |> Result.Error


    type ChartDescription =
        {
            Heading : string
            Text : string
        }


    let toDescription h t =
        {
            Heading = h
            Text = t
        }


    let toEmbeddedHtmlWithDescription (description : ChartDescription) (gChart : GenericChart) =
        let plotlyRef = PlotlyJSReference.Full

        let displayOpts =
            DisplayOptions.init(
                AdditionalHeadTags = [
                    script [_src description.Heading] []
                ],
                // Description = [
                //     h1 [] [str description.Heading]
                //     h2 [] [str description.Text]
                // ],
                PlotlyJSReference = plotlyRef
            )

        let result =
            gChart
            |> Chart.withDisplayOptions(displayOpts)
            |> GenericChart.toEmbeddedHTML

        result


    let toHtmlFileName (FileName fileName) =
        if fileName.EndsWith(".html", StringComparison.OrdinalIgnoreCase) then fileName
        else fileName + ".html"
        |> FileName


    let getHtmlChart fileName d ch =
        {
            textContent = toEmbeddedHtmlWithDescription d ch
            fileName = toHtmlFileName fileName
        }
        |> TextResult
