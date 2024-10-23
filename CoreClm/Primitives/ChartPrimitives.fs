namespace Primitives

open System.IO
open Plotly.NET
open Plotly.NET.GenericChart
open Giraffe.ViewEngine
open Softellect.Sys.Primitives

module ChartPrimitives =

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
        // let chartMarkup =
        //     toChartHTML gChart
        //
        // HTML.doc
        //     .Replace("[CHART]", chartMarkup)
        //     .Replace("[DESCRIPTION]", description.Text)
        //     .Replace("[ADDITIONAL_HEAD_TAGS]", description.Heading)

        let plotlyRef = PlotlyJSReference.Full

        let displayOpts =
            DisplayOptions.init(
                AdditionalHeadTags = [
                    script [_src description.Heading] []
                ],
                Description = [
                    h1 [] [str description.Heading]
                    h2 [] [str description.Text]
                    // h1 [] [str description.Text]
                ],
                PlotlyJSReference = plotlyRef
            )

        let result =
            gChart
            |> Chart.withDisplayOptions(displayOpts)
            |> toEmbeddedHTML

        result


    let getChart fileName d ch =
        {
            htmlContent = toEmbeddedHtmlWithDescription d ch
            fileName = fileName
        }
        |> HtmlChart
