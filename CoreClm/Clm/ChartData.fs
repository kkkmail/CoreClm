namespace Clm

open System
open Softellect.Sys.Core
open Clm.Substances
open Clm.ModelParams
open ClmSys.ContGenPrimitives
open Clm.CalculationData
open ClmSys.SolverData
open ClmSys.SolverRunnerPrimitives

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
            eeData : EeData
        }

        static member create (i : BinaryInfo) t xInput eeData =
            let x = makeNonNegative xInput
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
                        minData = xInput |> Array.min // Here we want to watch for negative values in the input data.
                        foodData = Option.bind (fun i -> x.[i] |> Some) foodIdx
                        wasteData = Option.bind (fun i -> x.[i] |> Some) wasteIdx
                        levelData = [| for level in 1..i.maxPeptideLength.length -> levelData level |]
                    }

                sugarData = z
                sugarEe = eeZ
                eeData = eeData
            }

        static member defaultValue =
            {
                t = 0.0
                aminoAcidsData = [| 0.0 |]
                enantiomericExcess = [| 0.0 |]
                totalSubst =
                    {
                        totalData = 0.0
                        minData = 0.0
                        foodData = None
                        wasteData = None
                        levelData = [| 0.0 |]
                    }
                sugarData = None
                sugarEe = None
                eeData = EeData.defaultValue
            }

        member csd.maxEe =
            csd.enantiomericExcess
            |> Array.map abs
            |> Array.max


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
            | Some c -> c.t
            | None -> 0.0
            |> decimal

        member cd.progress =
            let tEnd = cd.initData.tEnd
            min (max (if tEnd > 0.0m then cd.tLast / tEnd else 0.0m) 0.0m) 1.0m

        member cd.eeData =
            match cd.allChartData |> List.tryHead with
            | Some c -> c.eeData
            | None -> EeData.defaultValue


    type ChartDataUpdater () =
        interface IUpdater<ChartInitData, ChartSliceData, ChartData> with
            member _.init i = ChartData.create i
            member _.add a m = { m with allChartData = a :: m.allChartData }
