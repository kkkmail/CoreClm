namespace Clm.Generator

open System
open System.IO
open FSharp.Collections

open Clm.Distributions
open Clm.Substances
open Clm.Reactions
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRates
open Clm.ModelParams
open ClmSys.GeneralData
open Clm.CalculationData
open Clm.Generator.FSharpCodeExt
open Clm.Generator.ClmModelData
open ClmImpure.RateProvider
open ClmSys.ContGenPrimitives
open ClmSys.GeneralErrors
open ClmSys.ClmErrors

module ClmModel =

    type ClmModel (modelParams : ModelGenerationParams, modelDataId : ModelDataId, clmTaskId : ClmTaskId) =
        let rnd = RandomValueGetter.create()
        let generationType = RandomChoice
        let reactionShift = reactionShift modelParams.updateFuncType
        let seedValue = rnd.seed

        let rrp =
            {
                rateParams = modelParams.reactionRateModelParams
                successNumberType = modelParams.successNumberType
            }

        let si =
            {
                maxPeptideLength = modelParams.maxPeptideLength
                numberOfAminoAcids = modelParams.numberOfAminoAcids
                sedDirInfo = SedDirInfo.defaultValue
            }
            |> SubstInfo.create

        let rateProvider = ReactionRateProvider (rrp, si)
        let bf = RateGenerationData.create rnd generationType rateProvider si modelParams.successNumberType

        let modelInfo =
            {
                fileStructureVersion = modelParams.fileStructureVersion
                versionNumber = modelParams.versionNumber
                modelDataId = modelDataId
                numberOfSubstances = si.allSubst.Length
                numberOfAminoAcids = modelParams.numberOfAminoAcids
                maxPeptideLength = modelParams.maxPeptideLength
                seedValue = seedValue
                clmDefaultValueId = modelParams.clmDefaultValueId
            }

        let noOfRawReactions n = bf.noOfRawReactions n
        let getReactions n = bf.getReactions rnd rateProvider n

        let allReac =
            ReactionName.all
            |> List.map (fun e -> getReactions e)
            |> List.concat
            |> List.distinct

        let kW = (SedimentationAllReaction |> SedimentationAll |> rateProvider.getRates rnd generationType).forwardRate

        let allReacMap =
            allReac
            |> List.map (fun e -> e, e.fullName)
            |> Map.ofList

        let substToString s = si.allNamesMap.[s]
        let reactToString r = allReacMap.[r]
        let lstToString (l : list<Substance * int>) =
            l
            |> List.map (fun (s, n) -> (if n = 1 then "" else n.ToString() + " ") + (substToString s))
            |> String.concat " + "

        let xName = "x"
        let xSumName = "xSum"
        let xSumNameN = "xSumN"
        let xSumSquaredNameN = "xSumSquaredN"
        let dName = "d"

        let coeffSedAllName = "kW"

        let substComment (s : Substance) shift = shift + "    // " + (si.allInd.[s]).ToString() + " - " + (substToString s) + Nl
        let x (s : Substance) = xName + ".[" + (si.allInd.[s]).ToString() + "]"
        let d (s : Substance) = dName + "" + (si.allInd.[s]).ToString()

        let rate (l : list<Substance * int>) (ReactionRate r) =
            let toPown s n =
                match n with
                | 0 -> "1.0"
                | 1 -> x s
                | _ -> "(pown " + (x s) + " " + n.ToString() + ")"

            let a = l |> List.fold(fun acc (s, n) -> acc + (if acc <> "" then " * " else "") + (toPown s n)) ""
            (r.ToString() |> toFloat) + " * " + a + " // " + (lstToString l)

        let toMult i =
            match i with
            | 1 -> String.Empty
            | _ -> i.ToString() + ".0 * "

        let processReaction (r : AnyReaction) : list<Substance * string> =
            let update i o r f rc : list<Substance * string> =
                let catalysts =
                    (o |> List.map (fun (s, n) -> s, -n)) @ i
                    |> List.groupBy (fun (s, _) -> s)
                    |> List.map (fun (s, e) -> (s, e |> List.fold (fun acc (_, n) -> acc + n) 0))
                    |> List.filter (fun (_, n) -> n = 0)
                    |> List.map (fun (s, _) -> s)
                    |> Set.ofList

                let totalShift = reactionShift + "            "
                let (iSign, oSign) = if f then "-", "" else "", "-"
                let fwd = rate i r
                (
                    i
                    |> List.filter (fun (s, _) -> catalysts.Contains s |> not)
                    |> List.map (fun (s, n) -> (s, (totalShift + iSign + (toMult n) + fwd + " | " + rc + Nl)))
                )
                @
                (
                    o
                    |> List.filter (fun (s, _) -> catalysts.Contains s |> not)
                    |> List.map (fun (s, n) -> (s, (totalShift + oSign + (toMult n) + fwd+ " | " + rc + Nl)))
                )

            let rc = reactToString r

            match r with
            | Forward f -> update f.reaction.info.input f.reaction.info.output f.forwardRate true rc
            | Backward b -> update b.reaction.info.input b.reaction.info.output b.backwardRate false rc
            | Reversible rv ->
                (update rv.reaction.info.input rv.reaction.info.output rv.forwardRate true rc)
                @
                (update rv.reaction.info.output rv.reaction.info.input rv.backwardRate true rc)

        let generateTotals () =
            let g a =
                si.allSubst
                |> List.map (fun s -> match s.noOfAminoAcid a with | Some i -> Some (s, i) | None -> None)
                |> List.choose id
                |> List.map (fun (s, i) -> "                    " + (toMult i) + (x s) + " // " + (substToString s))

            let gg (v : list<string>) =
                let a = v |> String.concat Nl
                "                [|" + Nl + a + Nl + "                |]" + Nl + "                |> Array.sum" + Nl

            let gg1 ((a : AminoAcid), l, r) =
                "            // " + a.name + Nl + "            (" + Nl + (gg l) + "                ," + Nl + (gg r) + "            )" + Nl

            let y =
                si.aminoAcids
                |> List.map (fun a -> a, L a |> g, R a |> g)
                |> List.map (fun (a, l, r) -> gg1 (a, l, r))

            let x =
                y
                |> String.concat Nl

            "    let getTotals (x : array<double>) = " + Nl +
            "        [|" + Nl +
            x +
            "        |]"

        let generateTotalSubst() =
            let x =
                si.allSubst
                |> List.map (fun s -> s, s.atoms)
                |> List.map (fun (s, i) -> "            " + (toMult i) + (x s) + " // " + (substToString s))
                |> String.concat Nl

            "    let getTotalSubst (x : array<double>) = " + Nl +
            "        [|" + Nl +
            x +
            Nl + "        |]" + Nl + "        |> Array.sum" + Nl

        let modelDataParamsWithExtraData : ModelDataParamsWithExtraData =
            {
                regularParams =
                    {
                        modelDataParams =
                            {
                                modelInfo = modelInfo
                                allParams = rrp.allParams() |> Array.ofList
                            }
                        allSubstData =
                            {
                                allSubst = si.allSubst
                                allInd = si.allInd
                                allRawReactions = ReactionName.all |> List.map (fun n -> n, noOfRawReactions n)
                                allReactions = allReac |> List.groupBy (fun r -> r.name) |> List.map (fun (n, l) -> (n, int64 l.Length))
                            }
                    }

                funcParams =
                    {
                        getTotals = fun _ -> [||]
                        getTotalSubst = fun _ -> 0.0
                        getDerivative = fun _ -> [||]
                    }
            }

        let generate () =
            let t0 = DateTime.Now
            printfn "t0 = %A" t0
            let r0 = allReac |> List.map (fun r -> processReaction r)
            printfn "r0.Length = %A" r0.Length
            let t11 = DateTime.Now
            printfn "t11 = %A" t11
            printfn "t11 - t0 = %A" (t11 - t0).TotalSeconds


            let reactions =
                r0
                |> List.concat
                |> List.groupBy (fun (s, _) -> s)
                |> Map.ofList


            let t1 = DateTime.Now
            printfn "t1 = %A" t1
            printfn "t1 - t11 = %A" (t1 - t11).TotalSeconds


            let getReaction s =
                match reactions.TryFind s with
                | Some r -> r |> List.rev |> List.map (fun (_, e) -> e) |> String.concat String.Empty
                | None -> String.Empty


            let getTotalSedReac (s : Substance) shift =
                match kW with
                | Some (ReactionRate _) ->
                    match s with
                    | Simple h ->
                        match h with
                        | Abundant -> String.Empty
                        | Food -> String.Empty
                        | Waste -> Nl + shift + "            " + coeffSedAllName + " * (2.0 * " + xSumName + " * " + xSumNameN + " - " + xSumSquaredNameN + ")"
                    | _ -> Nl + shift + "            " + "-" + coeffSedAllName + " * (2.0 * " + xSumName + " - " + (x s) + ") * " + (x s)
                | None -> String.Empty


            let coeffSedAllCode =
                match kW with
                | Some (ReactionRate k) ->
                    "    let " + coeffSedAllName + " = " + k.ToString() + " / " + (si.allSubst.Length - 1).ToString() + ".0" + Nl
                | None -> String.Empty


            let a =
                si.allSubst
                |> List.map (fun s -> Nl + "    " + (substComment s "    ") +  "            [|" + Nl + (getTotalSedReac s "    ") + Nl + (getReaction s) + "            |]" + Nl + "            |> Array.sum" + Nl)


            let dInitCode xPar =
                let shift, g =
                    match xPar with
                    | "" -> "    ", d
                    | _ ->
                        let d1 s =
                            (d s) + " (" + xName + " : array<double>) " + xSumName + " " + xSumNameN + " " + xSumSquaredNameN
                        String.Empty, d1

                si.allSubst
                |> List.map (fun s -> Nl + (substComment s shift) + shift + "    let " + (g s) + " = " + Nl + shift + "        [|" + (getTotalSedReac s shift) + Nl + (getReaction s) + shift + "        |]" + Nl + shift + "        |> Array.sum" + Nl)


            let dArrayCode xPar =
                let shift, g =
                    match xPar with
                    | "" -> "    ", d
                    | _ ->
                        let d1 s = (d s) + " " + xPar + " " + xSumName + " " + xSumNameN + " " + xSumSquaredNameN
                        "", d1

                si.allSubst |> List.map (fun s -> shift + "            " + (g s)) |> String.concat Nl


            let t2 = DateTime.Now
            printfn "t2 = %A" t2
            printfn "t2 - t1 = %A" (t2 - t1).TotalSeconds
            let totalCode = generateTotals()
            let totalSubstCode = generateTotalSubst()


            let sc =
                si.allSubst
                |> List.filter (fun s -> not s.isSimple)
                |> List.map (fun s -> "                " + (s.atoms.ToString()) + ".0 * " + (x s) + " // " + (substToString s))
                |> String.concat Nl


            let sc2 =
                si.allSubst
                |> List.filter (fun s -> not s.isSimple)
                |> List.map (fun s -> "                " + (s.atoms.ToString()) + ".0 * " + (x s) + " * " + (x s) + " // " + (substToString s))
                |> String.concat Nl


            let sumCode = "        let " + xSumName + " = (" + xName + " |> Array.sum) - (" + (x Substance.food) + " + " + (x Substance.waste) + " + " + (x Substance.abundant) + ")" + Nl + Nl
            let sumCodeN = "        let " + xSumNameN + " = " + Nl + "            [|" + Nl + sc + Nl + "            |]" + Nl + "            |> Array.sum" + Nl + Nl
            let sumSquaredCodeN = "        let " + xSumSquaredNameN + " = " + Nl + "            [|" + Nl + sc2 + Nl + "            |]" + Nl + "            |> Array.sum" + Nl
            let shift = "                    "
            let modelDataParamsCode = "    let modelDataParamsWithExtraData =" + Nl + (modelDataParamsWithExtraData.toFSharpCode shift)


            let updateOuterCode =
                match modelParams.updateFuncType with
                | UseArray -> []
                | UseVariables -> []
                | UseFunctions ->
                    dInitCode xName


            let updateInnerCode =
                match modelParams.updateFuncType with
                | UseArray ->
                    [ "        [|" ]
                    @
                    a
                    @
                    [ "        |]" + Nl ]
                | UseVariables ->
                    dInitCode String.Empty
                    @
                    [
                        "        // printfn \"update::Assembling d...\"" + Nl
                        "        let d = "
                        "            [|"
                        dArrayCode String.Empty
                        "            |]" + Nl
                        "        // printfn \"update::Completed.\"" + Nl
                        "        d" + Nl
                    ]
                | UseFunctions ->
                    [ "        [|" ]
                    @
                    [ dArrayCode xName ]
                    @
                    [ "        |]" + Nl ]


            let updateCode =
                updateOuterCode
                @
                [
                    Nl
                    "    let update (xRaw : array<double>) : array<double> = "
                    "        // printfn \"update::Starting...\""
                    "        let x = xRaw |> Array.map (fun e -> max e 0.0)"
                    sumCode
                    sumCodeN
                    sumSquaredCodeN
                ]
                @
                updateInnerCode


            let paramCode =
                "    let seedValue = " + seedValue.ToString() + Nl +
                "    let numberOfAminoAcids = NumberOfAminoAcids." + (modelParams.numberOfAminoAcids.ToString()) + Nl +
                "    let maxPeptideLength = MaxPeptideLength." + (modelParams.maxPeptideLength.ToString()) + Nl +
                "    let numberOfSubstances = " + (si.allSubst.Length).ToString() + Nl +
                generateSubst() +
                coeffSedAllCode


            [
                "namespace Clm.Model" + Nl
                "open System"
                "open Clm.Substances"
                "open Clm.Distributions"
                "open Clm.ModelParams"
                "open Clm.ReactionTypes"
                "open Clm.ReactionRatesBase"
                "open Clm.ReactionRates"
                "open Clm.ReactionRateParams"
                "open Clm.CalculationData"
                "open ClmSys.ContGenPrimitives" + Nl
                "module ModelData = "
                paramCode + Nl
                totalSubstCode + Nl
                totalCode + Nl
            ]
            @ updateCode
            @ [ modelDataParamsCode ]


        let generateAndSave() =
            try
                printfn "Generating..."
                let s = generate()

                printfn "Writing..."
                File.WriteAllLines(DefaultModelDataFile, s)
                printfn "Done."

                Ok s
            with
            | e ->
                printfn "Exception occurred: %A" e
                e |> WriteFileExn |> FileErr |> Error


        let getModelDataImpl clmTaskId =
            {
                modelDataId = modelDataId
                clmTaskInfo =
                    {
                        clmTaskId = clmTaskId
                        numberOfAminoAcids = modelParams.numberOfAminoAcids
                        maxPeptideLength = modelParams.maxPeptideLength
                        clmDefaultValueId = modelParams.clmDefaultValueId
                    }
                data =
                {
                    seedValue = Some seedValue
                    fileStructureVersion = modelParams.fileStructureVersion

                    modelData =
                        {
                            modelDataParams =
                                {
                                    modelInfo = modelInfo
                                    allParams = rateProvider.providerParams.allParams() |> Array.ofList
                                }

                            modelBinaryData =
                                {
                                    calculationData = ModelCalculationData.create si allReac

                                    allRawReactions =
                                        ReactionName.all
                                        |> List.map (fun n -> n, noOfRawReactions n)

                                    allReactions =
                                        allReac
                                        |> List.groupBy (fun r -> r.name)
                                        |> List.map (fun (n, l) -> (n, int64 l.Length))
                                }
                        }
                }
            }


        member model.allSubstances = si.allSubst
        member model.allReactions = allReac
        member model.generateCode() = generateAndSave()
        member model.getModelData() = getModelDataImpl clmTaskId
