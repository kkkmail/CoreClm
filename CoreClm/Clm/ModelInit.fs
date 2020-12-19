namespace Clm

open FSharp.Collections

open Clm.Distributions
open Clm.Substances
open Clm.ModelParams


module ModelInit =

    type ModelInitValuesParams =
        {
            modelDataParams : ModelDataParamsWithExtraData
            distr : Distribution
            eeDistr : EeDistribution
            multiplier : double option
            multEe : double option
            useAbundant : bool
        }

        static member defaultMult = 0.001
        static member defaultMultEe = 0.001

        static member getDefaultValue p a =
            {
                modelDataParams = p
                distr = Distribution.createUniform DistributionParams.defaultValue
                eeDistr = EeDistribution.createSymmetricTriangular()
                multiplier = None
                multEe = None
                useAbundant = a
            }


    let defaultInit rnd (p : ModelInitValuesParams) y0 =
        let mult =
            match p.multiplier with
            | Some m -> m
            | None -> ModelInitValuesParams.defaultMult

        let allIndRev =
            p.modelDataParams.regularParams.allSubstData.allInd
            |> Map.toList
            |> List.map (fun (s, i) -> (i, s))
            |> Map.ofList

        let multEe =
            match p.multEe with
            | Some m -> m
            | None -> ModelInitValuesParams.defaultMultEe

        let allSubst = p.modelDataParams.regularParams.allSubstData.allSubst

        let nextValue _ =
            y0 * mult * (p.distr.nextDouble rnd) / (double (p.modelDataParams.regularParams.modelDataParams.modelInfo.numberOfSubstances - 1))

        let nextEe = multEe * (p.eeDistr.nextDouble rnd)

        let initVals =
            allSubst
            |> List.filter (fun s -> not s.isSimple)
            |> List.map (fun s -> orderPairs (s.aminoAcids, s.enantiomer.aminoAcids) |> fst |> Substance.fromList)
            |> List.distinct
            |> List.map (fun s -> (s, (nextValue s, nextEe)))

        let initValsMap = initVals |> Map.ofList
        let total = initVals |> List.map (fun (s, (v, _)) -> v * (double s.atoms)) |> List.sum

        let getValue i =
            let s = allIndRev.[i]
            match s with
            | Simple i ->
                match i with
                | Abundant ->
                    match p.useAbundant with
                    | true -> 1.0
                    | false -> 0.0
                | Food -> y0 - 2.0 * total
                | Waste -> 0.0
            | _ ->
                match initValsMap.TryFind s, initValsMap.TryFind s.enantiomer with
                | Some _, Some _ -> 0.0
                | Some (v, e), None -> v * (1.0 + e)
                | None, Some (v, e) -> v * (1.0 - e)
                | None, None -> 0.0

        [| for i in 0..(p.modelDataParams.regularParams.modelDataParams.modelInfo.numberOfSubstances - 1) -> getValue i |]
