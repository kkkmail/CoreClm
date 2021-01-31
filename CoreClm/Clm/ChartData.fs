namespace Clm

open Softellect.Sys.Core
open Clm.Substances
open Clm.ModelParams
open ClmSys.ContGenPrimitives

module ChartData =

    type TotalSubstData =
        {
            totalData : double
            minData : double
            foodData : double option
            wasteData : double option
            levelData : array<double>
        }


    type ChartType =
        | PlotAminoAcids
        | PlotEnantiomericExcess
        | PlotTotalSubst


    type ChartInitData =
        {
            modelDataId : ModelDataId
            defaultValueId : ClmDefaultValueId
            binaryInfo : BinaryInfo
            y0 : decimal
            tEnd : decimal
            description : string option
        }


    type ChartSliceData =
        {
            t : double
            aminoAcidsData : array<double>
            enantiomericExcess : array<double>
            totalSubst : TotalSubstData
            sugarData : double option
            sugarEe : double option
        }

        static member create (i : BinaryInfo) t x =
            let totals = i.getTotals x

            let getCorrectLD li di =
                let l = if li > 0.0 then li else 0.0
                let d = if di > 0.0 then di else 0.0
                (l, d)

            let getTotal li di =
                let (l, d) = getCorrectLD li di
                l + d

            let getEe li di =
                let (l, d) = getCorrectLD li di
                if (l + d) > 0.0 then (l - d) / (l + d) else 0.0

            let getZ f = i.allSubstData.allInd.TryFind (f Z |> ChiralSug) |> Option.bind (fun i -> x.[i] |> Some)

            let (z, eeZ) =
                    match getZ Ls, getZ Rs with
                    | Some l, Some d -> getTotal l d |> Some, getEe l d |> Some
                    | _ -> None, None

            {
                t = t
                aminoAcidsData = totals |> List.map (fun (l, d) -> getTotal l d) |> Array.ofList
                enantiomericExcess = totals |> List.map (fun (l, d) -> getEe l d) |> Array.ofList

                totalSubst =
                    let foodIdx = i.allSubstData.allInd.TryFind (AchiralSubst.Food |> Simple)
                    let wasteIdx = i.allSubstData.allInd.TryFind (AchiralSubst.Waste |> Simple)

                    let levelData level =
                        let levelSubst =
                            i.allSubstData.allSubst
                            |> List.filter (fun s -> s.length = level)
                            |> List.map (fun s -> i.allSubstData.allInd.[s])

                        levelSubst |> List.sumBy (fun i -> (double level) * x.[i])

                    {
                        totalData = i.getTotalSubst x
                        minData = x |> Array.min
                        foodData = Option.bind (fun i -> x.[i] |> Some) foodIdx
                        wasteData = Option.bind (fun i -> x.[i] |> Some) wasteIdx
                        levelData = [| for level in 1..i.maxPeptideLength.length -> levelData level |]
                    }

                sugarData = z
                sugarEe = eeZ
            }


    type ChartData =
        {
            initData : ChartInitData
            allChartData : list<ChartSliceData>
        }

        static member create i =
            {
                initData = i
                allChartData = []
            }

        member cd.maxEe =
            cd.allChartData
            |> List.map (fun e -> e.enantiomericExcess |> List.ofArray)
            |> List.concat
            |> List.map (fun e -> abs e)
            |> List.max

        member cd.maxAverageEe =
            match cd.allChartData with
            | [] -> 0.0
            | h :: _ ->
                let getData i = cd.allChartData |> List.map (fun e -> e.enantiomericExcess.[i])

                h.enantiomericExcess
                |> Array.mapi (fun i _ -> getData i)
                |> Array.map (fun e -> List.average e |> abs)
                |> Array.max

        member cd.maxWeightedAverageAbsEe =
            match cd.allChartData with
            | [] -> 0.0
            | h :: _ ->
                let weightFun i = double i

                let totalWeight =
                    cd.allChartData
                    |> List.mapi(fun i _ -> weightFun i)
                    |> List.sum

                let weigh i e = e |> Array.map (fun x -> (weightFun i) * (abs x) / totalWeight)

                let ee =
                    cd.allChartData
                    |> List.rev
                    |> List.mapi (fun i e -> weigh i e.enantiomericExcess)

                let getData i = ee |> List.map (fun e -> e.[i])

                h.enantiomericExcess
                |> Array.mapi (fun i _ -> getData i)
                |> Array.map List.sum
                |> Array.max

        member cd.maxLastEe =
            match cd.allChartData with
            | [] -> 0.0
            | h :: _ ->
                h.enantiomericExcess
                |> Array.map (fun e -> abs e)
                |> Array.max

        /// Last calculated value of tEnd.
        member cd.tLast =
            match cd.allChartData |> List.tryHead with
            | Some c -> c.t
            | None -> 0.0
            |> decimal

        member cd.progress =
            let tEnd = cd.initData.tEnd
            min (max (if tEnd > 0.0m then cd.tLast / tEnd else 0.0m) 0.0m) 1.0m


    type ChartDataUpdater () =
        interface IUpdater<ChartInitData, ChartSliceData, ChartData> with
            member __.init i = ChartData.create i
            member __.add a m = { m with allChartData = a :: m.allChartData }
