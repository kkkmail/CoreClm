namespace Analytics

open Clm.Substances
open Microsoft.FSharp.Core

open Clm.ModelParams
open ClmSys.GeneralData
open Clm.ChartData
open Plotly.NET
open ChartExt

module Visualization =

    type Plotter(i : PlotDataInfo, p : ChartData) =
        let getFileName (ct : ChartType) = ct.getFileName (i, p)
        let noOfOutputPoints = p.allChartData.Length - 1
        let allChartData = p.allChartData |> List.rev |> Array.ofList
        let minMax = (0.0, float p.tLast)
        let fn = [ for i in 0..(p.initData.binaryInfo.aminoAcids.Length - 1) -> i ]
        let tIdx = [ for i in 0..noOfOutputPoints -> i ]
        let xAxisName = "t"

        let showHtmlChart show chart =
            match show with
            | true -> showHtmlChart chart
            | false -> Ok()


        let description h =
            [
                "model name", p.initData.modelDataId.value |> toModelName
                "default id", $"%A{p.initData.defaultValueId.value}"
                "y0", $"%A{p.initData.y0}"
                "number of amino acids", $"%A{p.initData.binaryInfo.aminoAcids.Length}"
                "max peptide length", $"%A{p.initData.binaryInfo.maxPeptideLength.length}"
                "number of substances", $"%A{p.initData.binaryInfo.allSubstData.allSubst.Length}"
            ]
            @
            (p.initData.binaryInfo.allSubstData.allReactions |> List.map (fun (r, c) -> r.name, c.ToString()))
            @
            (p.initData.binaryInfo.allSubstData.allRawReactions |> List.map (fun (r, c) -> r.name + " (raw)", c.ToString()))
            @
            [
                "\ndescription", p.initData.description |> Option.defaultValue EmptyString
            ]
            |> List.map (fun (n, d) -> n + ": " + d)
            |> List.map (fun e -> e.Replace("\n", "<br>"))
            |> String.concat ", "
            |> toDescription h


        let getAminoAcidsImpl () =
            let getName (s : string) = s.ToUpper() + " + " + s.ToLower()
            let name (i : int) = getName (AminoAcid.toString i)
            let getFuncData i = tIdx |> List.map (fun t -> allChartData.[t].t, allChartData.[t].aminoAcidsData.[i])
            let fileName = getFileName PlotAminoAcids
            let zName = getName Z.name

            let sugarData =
                match tIdx |> List.map (fun t -> Option.bind (fun d -> Some (allChartData.[t].t, d)) allChartData.[t].sugarData) |> List.choose id with
                | [] -> None
                | x -> Some x

            let charts =
                (fn |> List.map (fun i -> Chart.Line(getFuncData i, Name = name i) |> Some))
                @ [ Option.bind (fun d -> Chart.Line(d, Name = zName) |> Some) sugarData ]
                |> List.choose id

            Chart.combine (charts)
            |> Chart.withXAxisStyle(xAxisName, MinMax = minMax)
            |> getChart fileName (description "Amino Acids")


        let getEnantiomericExcessImpl () =
            let fileName = getFileName PlotEnantiomericExcess

            let ldName (l : string) =
                let d = l.ToLower()
                "(" + l + " - " + d + ") / (" + l + " + " + d + ")"

            let name (i : int) = AminoAcid.toString i |> ldName
            let getFuncData i = tIdx |> List.map (fun t -> allChartData.[t].t, allChartData.[t].enantiomericExcess.[i])
            let zName = Z.name |> ldName

            let sugarEe =
                match tIdx |> List.map (fun t -> Option.bind (fun d -> Some (allChartData.[t].t, d)) allChartData.[t].sugarEe) |> List.choose id with
                | [] -> None
                | x -> Some x

            let charts =
                (fn |> List.map (fun i -> Chart.Line(getFuncData i, Name = name i) |> Some))
                @ [ Option.bind (fun d -> Chart.Line(d, Name = zName) |> Some) sugarEe ]
                |> List.choose id

            Chart.combine (charts)
            |> Chart.withXAxisStyle(xAxisName, MinMax = minMax)
            |> getChart fileName (description "Enantiomeric Excess")


        let getTotalSubstImpl () =
            let totalData = tIdx |> List.map (fun t -> allChartData.[t].t, allChartData.[t].totalSubst.totalData)
            let minData = tIdx |> List.map (fun t -> allChartData.[t].t, allChartData.[t].totalSubst.minData)
            let fileName = getFileName PlotTotalSubst

            let foodData =
                match tIdx |> List.map (fun t -> Option.bind (fun d -> Some (allChartData.[t].t, d)) allChartData.[t].totalSubst.foodData) |> List.choose id with
                | [] -> None
                | x -> Some x

            let wasteData =
                match tIdx |> List.map (fun t -> Option.bind (fun d -> Some (allChartData.[t].t, d)) allChartData.[t].totalSubst.wasteData) |> List.choose id with
                | [] -> None
                | x -> Some x

            let sugarData =
                match tIdx |> List.map (fun t -> Option.bind (fun d -> Some (allChartData.[t].t, d)) allChartData.[t].sugarData) |> List.choose id with
                | [] -> None
                | x -> Some x

            let levelData level = tIdx |> List.map (fun t -> allChartData.[t].t, allChartData.[t].totalSubst.levelData.[level])

            let charts =
                [ Chart.Line(totalData, Name = "Total") |> Some; Chart.Line(minData, Name = "Min") |> Some ]
                @ [ Option.bind (fun d -> Chart.Line(d, Name = AchiralSubst.Food.name)|> Some) foodData ]
                @ [ Option.bind (fun d -> Chart.Line(d, Name = AchiralSubst.Waste.name)|> Some) wasteData ]
                @ [ Option.bind (fun d -> Chart.Line(d, Name = Z.name) |> Some) sugarData ]
                @ [ for level in 0..p.initData.binaryInfo.maxPeptideLength.length - 1 -> Chart.Line(levelData level, Name = (level + 1).ToString()) |> Some ]
                |> List.choose id

            Chart.combine(charts)
            |> Chart.withXAxisStyle(xAxisName, MinMax = minMax)
            |> getChart fileName (description "Totals")


        member _.plotAminoAcids (show : bool) = getAminoAcidsImpl() |> showHtmlChart show
        member _.plotTotalSubst (show : bool) = getTotalSubstImpl() |> showHtmlChart show
        member _.plotEnantiomericExcess (show : bool) = getEnantiomericExcessImpl() |> showHtmlChart show

        member _.getAminoAcids () = getAminoAcidsImpl()
        member _.getTotalSubst () = getTotalSubstImpl()
        member _.getEnantiomericExcess () = getEnantiomericExcessImpl()
