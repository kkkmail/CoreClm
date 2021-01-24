namespace Clm.Generator

open Clm.Substances
open Clm.Distributions
open Clm.ReactionRatesBase
open Clm.ReactionRates
open Clm.ReactionRateParams
open Clm.ModelParams
open Clm.ReactionTypes
open ClmSys.DistributionData
open ClmSys.GeneralData
open ClmSys.ContGenPrimitives
open ClmSys.ModelData


module FSharpCodeExt =

    let increaseShift shift = shift + "    "
    let increaseShiftTwice shift = shift |> increaseShift |> increaseShift
    let toArray (arr: 'T [,]) = arr |> Seq.cast<'T> |> Seq.toArray

    type System.Int32
        with
        member this.toFSharpCode = this.ToString()


    type System.Int64
        with
        member this.toFSharpCode = this.ToString() + "L"


    type System.Guid
        with
        member this.toFSharpCode = "\"" + this.ToString() + "\""


    type System.Decimal
        with
        member this.toFSharpCode = this.ToString() + "m"


    let fold f state (arr: 'a [,]) =
        Seq.cast<'a> arr
        |> Seq.fold f state


    let toFloat (s : string) =
        match s.Contains(".") with
        | true -> s
        | false ->
            match s.ToUpper().Contains("E+") || s.ToUpper().Contains("E-") with
            | true -> s
            | false -> s + ".0"


    let doubleFSharpString (d : double) = d.ToString() |> toFloat


    let doubleOptFSharpString (d : double option) =
        match d with
        | Some v -> "Some " + (v.ToString() |> toFloat)
        | None -> "None"


    let arrayToFSharpString (a : double[]) (shift : string) =
        let s =
            a
            |> Array.map (fun e -> doubleFSharpString e)
            |> String.concat "; "
        shift + "[| " + s + " |]"


    let array2DToFSharpString (a : double[,]) (shift : string) =
        let arrayShift = shift |> increaseShift

        let s =
            [| for i in 0..((Array2D.length1 a) - 1) -> i |]
            |> Array.map (fun i -> a.[i,*])
            |> Array.map (fun e -> arrayToFSharpString e arrayShift)
            |> String.concat Nl

        shift + "[| " + Nl +
        s + Nl +
        shift + "|]" + Nl


    type Substance
        with
            member this.toFSharpCode shift = shift + (this.ToString())


    type DistributionParams

        with

        member this.toFSharpCode =
            "{ " +
            "threshold = " + (doubleOptFSharpString this.threshold) + "; " +
            "scale = " + (doubleOptFSharpString this.scale) + "; " +
            "shift = " + (doubleOptFSharpString this.shift) +
            " }"


    type DistributionParamsWithType
        with

        member this.toFSharpCode =
            "{ " +
            "distributionType = " + this.distributionType.ToString() + "; " +
            "distributionParams = " + this.distributionParams.toFSharpCode +
            " }"


    type Distribution
        with

        member this.toFSharpCode = this.value.toFSharpCode + " |> " + "Distribution"


    type EeDistribution
        with

        member distr.toFSharpCode =
            let (EeDistribution d) = distr
            d.toFSharpCode + " |> " + "EeDistribution"


    let toEeDistrOpt (distr : EeDistribution option) =
        match distr with
        | Some d -> d.toFSharpCode + " |> " + "Some"
        | None -> "None"


    type EeDistributionGetter
        with
        member distr.toFSharpCode =
            match distr with
            | NoneEeGetter -> "NoneEeGetter"
            | DeltaEeDistributionGetter -> "DeltaEeDistributionGetter"
            | CenteredEeDistributionGetter -> "CenteredEeDistributionGetter"


    type RateMultiplierDistribution
        with

        member distr.toFSharpCode =
            match distr with
            | RateMultDistr d -> d.toFSharpCode + " |> " + "RateMultDistr"
            | NoneRateMult -> "NoneRateMult"


    type RateMultiplierDistributionGetter
        with
        member distr.toFSharpCode =
            match distr with
            | NoneRateMultDistrGetter -> "NoneRateMultDistrGetter"
            | DeltaRateMultDistrGetter -> "DeltaRateMultDistrGetter"
            | TriangularRateMultDistrGetter -> "TriangularRateMultDistrGetter"
            | SymmetricTriangularRateMultDistrGetter -> "SymmetricTriangularRateMultDistrGetter"


    type CatRatesEeParam
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    rateMultiplierDistr = " + (p.rateMultiplierDistr.toFSharpCode) + Nl +
            shift + "    eeDistribution = " + (toEeDistrOpt p.eeDistribution) + Nl +
            shift + "}" + Nl


    type EnCatRatesEeParam
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    rateMultiplierDistr = " + (p.rateMultiplierDistr.toFSharpCode) + Nl +
            shift + "    enEeDistribution = " + (toEeDistrOpt p.enEeDistribution) + Nl +
            shift + "}" + Nl


    type AcCatRatesNoneEeParam
        with
        member p.toFSharpCode (shift : string) =
            shift + $"{nameof(AcCatRatesNoneEeParam)}" + Nl


    type AcCatRatesFwdEeParam
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    rateMultiplierDistr = " + (p.rateMultiplierDistr.toFSharpCode) + Nl +
            shift + "    acFwdEeDistribution = " + (toEeDistrOpt p.acFwdEeDistribution) + Nl +
            shift + "}" + Nl


    type AcCatRatesBkwEeParam
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    rateMultiplierDistr = " + (p.rateMultiplierDistr.toFSharpCode) + Nl +
            shift + "    acBkwEeDistribution = " + (toEeDistrOpt p.acBkwEeDistribution) + Nl +
            shift + "}" + Nl


    type AcCatRatesEeParam
        with
        member p.toFSharpCode (shift : string) =
            match p with
            | AcNoneParam p -> (p.toFSharpCode shift) + (shift + $"|> {nameof(AcNoneParam)}" + Nl)
            | AcForwardRateOnlyParam p -> (p.toFSharpCode shift) + (shift + $"|> {nameof(AcForwardRateOnlyParam)}" + Nl)
            | AcBackwardRateOnlyParam p -> (p.toFSharpCode shift) + (shift + $"|> {nameof(AcBackwardRateOnlyParam)}" + Nl)


    type FoodCreationParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    foodCreationRate = " + (doubleFSharpString p.foodCreationRate) + Nl +
            shift + "}" + Nl


    type WasteRemovalParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    wasteRemovalRate = " + (doubleFSharpString p.wasteRemovalRate) + Nl +
            shift + "}" + Nl


    type WasteRecyclingParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    wasteRecyclingRate = " + (doubleFSharpString p.wasteRecyclingRate) + Nl +
            shift + "}" + Nl


    type SynthesisRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    synthesisDistribution = " + p.synthesisDistribution.toFSharpCode + Nl +
            shift + "    forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "    backwardScale = " + (doubleOptFSharpString p.backwardScale) + Nl +
            shift + "}" + Nl


    type SynthesisParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | SynthRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + "SynthRndParam" + Nl)


    type SugarSynthesisRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    sugarSynthesisDistribution = " + p.sugarSynthesisDistribution.toFSharpCode + Nl +
            shift + "    forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "    backwardScale = " + (doubleOptFSharpString p.backwardScale) + Nl +
            shift + "}" + Nl


    type SugarSynthesisParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | SugarSynthRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + "SugarSynthRndParam" + Nl)


    type CatRatesSimGeneration
        with

        member p.toFSharpCode =
            match p with
            | DistributionBased d -> d.toFSharpCode + " |> DistributionBased"
            | FixedValue d -> d.toFSharpCode + " |> FixedValue"


    type CatRatesSimilarityParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    catRatesSimGeneration = " + p.catRatesSimGeneration.toFSharpCode + Nl +
            shift + "    getRateMultiplierDistr = " + p.getRateMultiplierDistr.toFSharpCode + Nl +
            shift + "    getEeDistr = " + p.getEeDistr.toFSharpCode + Nl +
            shift + "}" + Nl


    type CatalyticSynthesisRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    synthesisParam = " + Nl + (p.synthesisParam.toFSharpCode (increaseShiftTwice shift)) +
            shift + "    catSynthRndEeParams = " + Nl + (p.catSynthRndEeParams.toFSharpCode (increaseShiftTwice shift)) +
            shift + "}" + Nl


    type CatalyticSynthesisSimilarParam
        with
        member p.toFSharpCode (shift : string) =
            Nl +
            shift + "{" + Nl +
            shift + "    catSynthParam = " + Nl + (p.catSynthParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    catSynthSimParam = " + Nl + (p.catSynthSimParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "}" + Nl


    type CatalyticSynthesisParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | CatSynthRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + "CatSynthRndParam" + Nl)
            | CatSynthSimParam q -> (q.toFSharpCode shift) + (shift + "|> " + "CatSynthSimParam" + Nl)


    type EnCatRatesSimilarityParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    enCatRatesSimGeneration = " + p.enCatRatesSimGeneration.toFSharpCode + Nl +
            shift + "    getRateMultiplierDistr = " + p.getRateMultiplierDistr.toFSharpCode + Nl +
            shift + "    getForwardEeDistr = " + p.getForwardEeDistr.toFSharpCode + Nl +
            shift + "    getBackwardEeDistr = " + p.getBackwardEeDistr.toFSharpCode + Nl +
            shift + "}" + Nl


    type AcCatRatesSimilarityParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    acCatRatesSimGeneration = " + p.acCatRatesSimGeneration.toFSharpCode + Nl +
            shift + "    getRateMultiplierDistr = " + p.getRateMultiplierDistr.toFSharpCode + Nl +
            shift + "    getForwardEeDistr = " + p.getForwardEeDistr.toFSharpCode + Nl +
            shift + "    getBackwardEeDistr = " + p.getBackwardEeDistr.toFSharpCode + Nl +
            shift + "}" + Nl


    type EnCatalyticSynthesisRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    synthesisParam = " + Nl + (p.synthesisParam.toFSharpCode (increaseShiftTwice shift)) +
            shift + "    enCatSynthRndEeParams = " + Nl + (p.enCatSynthRndEeParams.toFSharpCode (increaseShiftTwice shift)) +
            shift + "}" + Nl


    type EnCatalyticSynthesisSimilarParam
        with
        member p.toFSharpCode (shift : string) =
            Nl +
            shift + "{" + Nl +
            shift + "    enCatSynthParam = " + Nl + (p.enCatSynthParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    enCatSynthSimParam = " + Nl + (p.enCatSynthSimParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "}" + Nl


    type AcCatalyticSynthesisRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    synthesisParam = " + Nl + (p.synthesisParam.toFSharpCode (increaseShiftTwice shift)) +
            shift + "    acCatSynthRndEeParams = " + Nl + (p.acCatSynthRndEeParams.toFSharpCode (increaseShiftTwice shift)) +
            shift + "}" + Nl


    type AcCatalyticSynthesisSimilarParam
        with
        member p.toFSharpCode (shift : string) =
            Nl +
            shift + "{" + Nl +
            shift + "    acCatSynthParam = " + Nl + (p.acCatSynthParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    acCatSynthSimParam = " + Nl + (p.acCatSynthSimParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "}" + Nl


    type EnCatalyticSynthesisParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | EnCatSynthRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + nameof(EnCatSynthRndParam) + Nl)
            | EnCatSynthSimParam q -> (q.toFSharpCode shift) + (shift + "|> " + nameof(EnCatSynthSimParam) + Nl)


    type AcCatalyticSynthesisParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | AcCatSynthRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + nameof(AcCatSynthRndParam) + Nl)
            | AcCatSynthSimParam q -> (q.toFSharpCode shift) + (shift + "|> " + nameof(AcCatSynthSimParam) + Nl)


    type DestructionRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    destructionDistribution = " + p.destructionDistribution.toFSharpCode + Nl +
            shift + "    forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "    backwardScale = " + (doubleOptFSharpString p.backwardScale) + Nl +
            shift + "}" + Nl


    type DestructionParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | DestrRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + "DestrRndParam" + Nl)


    type CatalyticDestructionRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    destructionParam = " + Nl + (p.destructionParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    catDestrRndEeParams = " + Nl + (p.catDestrRndEeParams.toFSharpCode (increaseShiftTwice shift)) +
            shift + "}" + Nl


    type CatalyticDestructionSimilarParam
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    catDestrParam = " + Nl + (p.catDestrParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    catDestrSimParam = " + Nl + (p.catDestrSimParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "}" + Nl


    type CatalyticDestructionParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | CatDestrRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + "CatDestrRndParam" + Nl)
            | CatDestrSimParam q -> (q.toFSharpCode shift) + (shift + "|> " + "CatDestrSimParam" + Nl)


    type EnCatalyticDestructionRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    destructionParam = " + Nl + (p.destructionParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    enCatDestrRndEeParams = " + Nl + (p.enCatDestrRndEeParams.toFSharpCode (increaseShiftTwice shift)) +
            shift + "}" + Nl


    type EnCatalyticDestructionSimilarParam
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    enCatDestrParam = " + Nl + (p.enCatDestrParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    enCatDestrSimParam = " + Nl + (p.enCatDestrSimParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "}" + Nl


    type AcCatalyticDestructionRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    destructionParam = " + Nl + (p.destructionParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    acCatDestrRndEeParams = " + Nl + (p.acCatDestrRndEeParams.toFSharpCode (increaseShiftTwice shift)) +
            shift + "}" + Nl


    type AcCatalyticDestructionSimilarParam
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    acCatDestrParam = " + Nl + (p.acCatDestrParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    acCatDestrSimParam = " + Nl + (p.acCatDestrSimParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "}" + Nl


    type EnCatalyticDestructionParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | EnCatDestrRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + nameof(EnCatDestrRndParam) + Nl)
            | EnCatDestrSimParam q -> (q.toFSharpCode shift) + (shift + "|> " + nameof(EnCatDestrSimParam) + Nl)


    type AcCatalyticDestructionParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | AcCatDestrRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + nameof(AcCatDestrRndParam) + Nl)
            | AcCatDestrSimParam q -> (q.toFSharpCode shift) + (shift + "|> " + nameof(AcCatDestrSimParam) + Nl)


    type SedDirRatesEeParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    sedDirRateMultiplierDistr = " + p.sedDirRateMultiplierDistr.toFSharpCode + Nl +
            shift + "    eeDistribution = " + (toEeDistrOpt p.eeDistribution) + Nl +
            shift + "}" + Nl



    type SedimentationDirectRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    sedDirDistribution = " + p.sedDirDistribution.toFSharpCode + Nl +
            shift + "    forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "    sedDirRatesEeParam =" + Nl + (p.sedDirRatesEeParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "}" + Nl


    type SedDirSimilarityParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    sedDirSimBaseDistribution = " + p.sedDirSimBaseDistribution.toFSharpCode + Nl +
            shift + "    getRateMultiplierDistr = " + p.getRateMultiplierDistr.toFSharpCode + Nl +
            shift + "    getEeDistr = " + p.getEeDistr.toFSharpCode + Nl +
            shift + "}" + Nl


    type SedimentationDirectSimilarParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    sedDirParam =" + Nl + (p.sedDirParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    sedDirSimParam =" + Nl + (p.sedDirSimParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "}" + Nl


    type SedimentationDirectParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | SedDirRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + "SedDirRndParam" + Nl)
            | SedDirSimParam q -> (q.toFSharpCode shift) + (shift + "|> " + "SedDirSimParam" + Nl)


    type SedimentationAllRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    sedimentationAllDistribution = " + p.sedimentationAllDistribution.toFSharpCode + Nl +
            shift + "    forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "}" + Nl


    type SedimentationAllParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | SedAllRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + "SedAllRndParam" + Nl)


    type LigationRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    ligationDistribution = " + p.ligationDistribution.toFSharpCode + Nl +
            shift + "    forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "    backwardScale = " + (doubleOptFSharpString p.backwardScale) + Nl +
            shift + "}" + Nl


    type LigationParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | LigRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + "LigRndParam" + Nl)


    type CatalyticLigationRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    ligationParam = " + Nl + (p.ligationParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    catLigRndEeParams = " + Nl + (p.catLigRndEeParams.toFSharpCode (increaseShiftTwice shift)) +
            shift + "}" + Nl


    type CatalyticLigationSimilarParam
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    catLigParam = " + Nl + (p.catLigParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    catLigSimParam = " + Nl + (p.catLigSimParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "}" + Nl


    type CatalyticLigationParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | CatLigRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + "CatLigRndParam" + Nl)
            | CatLigSimParam q -> (q.toFSharpCode shift) + (shift + "|> " + "CatLigSimParam" + Nl)


    type EnCatalyticLigationRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    ligationParam = " + Nl + (p.ligationParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    enCatLigRndEeParams = " + Nl + (p.enCatLigRndEeParams.toFSharpCode (increaseShiftTwice shift)) +
            shift + "}" + Nl


    type EnCatalyticLigationSimilarParam
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    enCatLigParam = " + Nl + (p.enCatLigParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    enCatLigSimParam = " + Nl + (p.enCatLigSimParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "}" + Nl


    type EnCatalyticLigationParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | EnCatLigRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + "EnCatLigRndParam" + Nl)
            | EnCatLigSimParam q -> (q.toFSharpCode shift) + (shift + "|> " + "EnCatLigSimParam" + Nl)


    type AcFwdCatalyticLigationRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    ligationParam = " + Nl + (p.ligationParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    acFwdCatLigRndEeParams = " + Nl + (p.acFwdCatLigRndEeParams.toFSharpCode (increaseShiftTwice shift)) +
            shift + "}" + Nl


    type AcFwdCatalyticLigationSimilarParam
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    acFwdCatLigParam = " + Nl + (p.acFwdCatLigParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    acFwdCatLigSimParam = " + Nl + (p.acFwdCatLigSimParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "}" + Nl


    type AcFwdCatalyticLigationParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | AcFwdCatLigRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + "AcFwdCatLigRndParam" + Nl)
            | AcFwdCatLigSimParam q -> (q.toFSharpCode shift) + (shift + "|> " + "AcFwdCatLigSimParam" + Nl)


    type AcBkwCatalyticLigationRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    ligationParam = " + Nl + (p.ligationParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    acBkwCatLigRndEeParams = " + Nl + (p.acBkwCatLigRndEeParams.toFSharpCode (increaseShiftTwice shift)) +
            shift + "}" + Nl


    type AcBkwCatalyticLigationSimilarParam
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    acBkwCatLigParam = " + Nl + (p.acBkwCatLigParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    acBkwCatLigSimParam = " + Nl + (p.acBkwCatLigSimParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "}" + Nl


    type AcBkwCatalyticLigationParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | AcBkwCatLigRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + "AcBkwCatLigRndParam" + Nl)
            | AcBkwCatLigSimParam q -> (q.toFSharpCode shift) + (shift + "|> " + "AcBkwCatLigSimParam" + Nl)


    type RacemizationRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    racemizationDistribution = " + p.racemizationDistribution.toFSharpCode + Nl +
            shift + "    forwardScale = " + (doubleOptFSharpString p.forwardScale) + Nl +
            shift + "}" + Nl


    type RacemizationParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | RacemRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + "RacemRndParam" + Nl)


    type CatalyticRacemizationRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    racemizationParam = " + Nl + (p.racemizationParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    catRacemRndEeParams = " + Nl + (p.catRacemRndEeParams.toFSharpCode (increaseShiftTwice shift)) +
            shift + "}" + Nl


    type CatalyticRacemizationSimilarParam
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    catRacemParam = " + Nl + (p.catRacemParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    catRacemSimParam = " + Nl + (p.catRacemSimParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "}" + Nl


    type CatalyticRacemizationParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | CatRacemRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + "CatRacemRndParam" + Nl)
            | CatRacemSimParam q -> (q.toFSharpCode shift) + (shift + "|> " + "CatRacemSimParam" + Nl)


    type EnCatalyticRacemizationRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    racemizationParam = " + Nl + (p.racemizationParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    enCatRacemRndEeParams = " + Nl + (p.enCatRacemRndEeParams.toFSharpCode (increaseShiftTwice shift)) +
            shift + "}" + Nl


    type EnCatalyticRacemizationSimilarParam
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    enCatRacemParam = " + Nl + (p.enCatRacemParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    enCatRacemSimParam = " + Nl + (p.enCatRacemSimParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "}" + Nl


    type EnCatalyticRacemizationParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | EnCatRacemRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + "EnCatRacemRndParam" + Nl)
            | EnCatRacemSimParam q -> (q.toFSharpCode shift) + (shift + "|> " + "EnCatRacemSimParam" + Nl)


    type AcCatalyticRacemizationRandomParam
        with

        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    racemizationParam = " + Nl + (p.racemizationParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    acCatRacemRndEeParams = " + Nl + (p.acCatRacemRndEeParams.toFSharpCode (increaseShiftTwice shift)) +
            shift + "}" + Nl


    type AcCatalyticRacemizationSimilarParam
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    acCatRacemParam = " + Nl + (p.acCatRacemParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    acCatRacemSimParam = " + Nl + (p.acCatRacemSimParam.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "}" + Nl


    type AcCatalyticRacemizationParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | AcCatRacemRndParam q -> (q.toFSharpCode shift) + (shift + "|> " + "AcCatRacemRndParam" + Nl)
            | AcCatRacemSimParam q -> (q.toFSharpCode shift) + (shift + "|> " + "AcCatRacemSimParam" + Nl)


    type ActivationParam
        with

        member p.toFSharpCode (shift : string) =
            match p with
            | ActivationRndParam q ->
                shift + "{" + Nl +
                shift + "    activationDistribution = " + q.activationDistribution.toFSharpCode + Nl +
                shift + "    forwardScale = " + (doubleOptFSharpString q.forwardScale) + Nl +
                shift + "}" + Nl +
                shift + "|> ActivationRndParam" + Nl


    type ReactionRateModelParam
        with

        member rm.toFSharpCode (shift : string) =
            match rm with
            | FoodCreationRateParam m -> (m.toFSharpCode shift) + shift + "|> FoodCreationRateParam" + Nl
            | WasteRemovalRateParam m -> (m.toFSharpCode shift) + shift + "|> WasteRemovalRateParam" + Nl
            | WasteRecyclingRateParam m -> (m.toFSharpCode shift) + shift + "|> WasteRecyclingRateParam" + Nl
            | SynthesisRateParam m -> (m.toFSharpCode shift) + shift + "|> SynthesisRateParam" + Nl
            | SugarSynthesisRateParam m -> (m.toFSharpCode shift) + shift + "|> SugarSynthesisRateParam" + Nl
            | DestructionRateParam m -> (m.toFSharpCode shift) + shift + "|> DestructionRateParam" + Nl
            | CatalyticDestructionRateParam m -> (m.toFSharpCode shift) + shift + "|> CatalyticDestructionRateParam" + Nl
            | EnCatalyticDestructionRateParam m -> (m.toFSharpCode shift) + shift + "|> EnCatalyticDestructionRateParam" + Nl
            | AcCatalyticDestructionRateParam m -> (m.toFSharpCode shift) + shift + "|> AcCatalyticDestructionRateParam" + Nl
            | CatalyticSynthesisRateParam m -> (m.toFSharpCode shift) + shift + "|> CatalyticSynthesisRateParam" + Nl
            | EnCatalyticSynthesisRateParam m -> (m.toFSharpCode shift) + shift + "|> EnCatalyticSynthesisRateParam" + Nl
            | AcCatalyticSynthesisRateParam m -> (m.toFSharpCode shift) + shift + "|> AcCatalyticSynthesisRateParam" + Nl
            | LigationRateParam m -> (m.toFSharpCode shift) + shift + "|> LigationRateParam" + Nl
            | CatalyticLigationRateParam m -> (m.toFSharpCode shift) + shift + "|> CatalyticLigationRateParam" + Nl
            | EnCatalyticLigationRateParam m -> (m.toFSharpCode shift) + shift + "|> EnCatalyticLigationRateParam" + Nl
            | AcFwdCatalyticLigationRateParam m -> (m.toFSharpCode shift) + shift + "|> AcFwdCatalyticLigationRateParam" + Nl
            | AcBkwCatalyticLigationRateParam m -> (m.toFSharpCode shift) + shift + "|> AcBkwCatalyticLigationRateParam" + Nl
            | SedimentationDirectRateParam m -> (m.toFSharpCode shift) + shift + "|> SedimentationDirectRateParam" + Nl
            | SedimentationAllRateParam m -> (m.toFSharpCode shift) + shift + "|> SedimentationAllRateParam" + Nl
            | RacemizationRateParam m -> (m.toFSharpCode shift) + shift + "|> RacemizationRateParam" + Nl
            | CatalyticRacemizationRateParam m -> (m.toFSharpCode shift) + shift + "|> CatalyticRacemizationRateParam" + Nl
            | EnCatalyticRacemizationRateParam m -> (m.toFSharpCode shift) + shift + "|> EnCatalyticRacemizationRateParam" + Nl
            | AcCatalyticRacemizationRateParam m -> (m.toFSharpCode shift) + shift + "|> AcCatalyticRacemizationRateParam" + Nl
            | ActivationRateParam m -> (m.toFSharpCode shift) + shift + "|> ActivationRateParam" + Nl


    type ReactionRateModelParamWithUsage
        with

        member rrmp.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    modelParam = " + Nl + (rrmp.modelParam.toFSharpCode (increaseShiftTwice shift) ) +
            shift + "    usage = " + rrmp.usage.ToString() + Nl +
            shift + "}" + Nl


    let toParamFSharpCode (allParams : array<ReactionRateModelParamWithUsage>) (shift : string) =
        allParams |> Array.map (fun e -> e.toFSharpCode shift) |> String.concat Nl


    type ReactionRateProviderParams
        with
        member rrp.toParamFSharpCode (shift : string) = toParamFSharpCode (rrp.allParams() |> Array.ofList) shift


    type ModelDataId
        with
        member p.toFSharpCode = "(" + p.value.toFSharpCode + " |> Guid |> ModelDataId)"


    type NumberOfAminoAcids
        with
        member p.toFSharpCode = "NumberOfAminoAcids" + "." + p.ToString()


    let getAminoAcidsCode (n : NumberOfAminoAcids) = "AminoAcid.getAminoAcids " + n.toFSharpCode


    type MaxPeptideLength
        with
        member p.toFSharpCode = "MaxPeptideLength" + "." + p.ToString()


    type ClmDefaultValueId
        with
        member p.toFSharpCode = "ClmDefaultValueId " + p.ToString()


    type ModelInfo
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    fileStructureVersion = " + p.fileStructureVersion.toFSharpCode + Nl +
            shift + "    versionNumber = " + "\"" + p.versionNumber + "\"" + Nl +
            shift + "    modelDataId = " + p.modelDataId.toFSharpCode + Nl +
            shift + "    numberOfSubstances = " + p.numberOfSubstances.toFSharpCode + Nl +
            shift + "    numberOfAminoAcids = " + p.numberOfAminoAcids.toFSharpCode + Nl +
            shift + "    maxPeptideLength = " + p.maxPeptideLength.toFSharpCode + Nl +
            shift + "    seedValue = " + p.seedValue.toFSharpCode + Nl +
            shift + "    clmDefaultValueId = " + p.clmDefaultValueId.toFSharpCode + Nl +
            shift + "}" + Nl


    type CollisionResolutionType
        with
        member p.toFSharpCode = $"{p}"


    type PairCollisionResolution
        with
        member p.toFSharpCode (shift : string) =
            match p with
            | PairCollision -> nameof(PairCollision)
            | EachInPair v ->
                Nl +
                shift + "{" + Nl +
                shift + "    collisionA = " + v.collisionA.toFSharpCode + Nl +
                shift + "    collisionB = " + v.collisionB.toFSharpCode + Nl +
                shift + "}" + Nl +
                shift + "|> " + nameof(EachInPair) + Nl


    type TripleCollisionResolution
        with
        member p.toFSharpCode (shift : string) =
            match p with
            | TripleCollision -> nameof(TripleCollision)
            | EachInTriple v ->
                Nl +
                shift + "{" + Nl +
                shift + "    collisionA = " + v.collisionA.toFSharpCode + Nl +
                shift + "    collisionB = " + v.collisionB.toFSharpCode + Nl +
                shift + "    collisionC = " + v.collisionC.toFSharpCode + Nl +
                shift + "}" + Nl +
                shift + "|> " + nameof(EachInTriple) + Nl


    type CollisionData
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    sugSynthColl = " + p.sugSynthColl.toFSharpCode(increaseShiftTwice shift) + Nl +
            shift + "    catSynthColl = " + p.catSynthColl.toFSharpCode(increaseShiftTwice shift) + Nl +
            shift + "    enCatSynthColl = " + p.enCatSynthColl.toFSharpCode(increaseShiftTwice shift) + Nl +
            shift + "    acCatSynthColl = " + p.acCatSynthColl.toFSharpCode(increaseShiftTwice shift) + Nl +
            shift + "    catDestrColl = " + p.catDestrColl.toFSharpCode(increaseShiftTwice shift) + Nl +
            shift + "    enCatDestrColl = " + p.enCatDestrColl.toFSharpCode(increaseShiftTwice shift) + Nl +
            shift + "    acCatDestrColl = " + p.acCatDestrColl.toFSharpCode(increaseShiftTwice shift) + Nl +
            shift + "    catLigColl = " + p.catLigColl.toFSharpCode(increaseShiftTwice shift) + Nl +
            shift + "    enCatLigColl = " + p.enCatLigColl.toFSharpCode(increaseShiftTwice shift) + Nl +
            shift + "    acFwdCatLigColl = " + p.acFwdCatLigColl.toFSharpCode(increaseShiftTwice shift) + Nl +
            shift + "    acBkwCatLigColl = " + p.acBkwCatLigColl.toFSharpCode(increaseShiftTwice shift) + Nl +
            shift + "    catRacemColl = " + p.catRacemColl.toFSharpCode(increaseShiftTwice shift) + Nl +
            shift + "    enCatRacemColl = " + p.enCatRacemColl.toFSharpCode(increaseShiftTwice shift) + Nl +
            shift + "    acCatRacemColl = " + p.acCatRacemColl.toFSharpCode(increaseShiftTwice shift) + Nl +
            shift + "    sedDirColl = " + p.sedDirColl.toFSharpCode(increaseShiftTwice shift) + Nl +
            shift + "    acColl = " + p.acColl.toFSharpCode(increaseShiftTwice shift) + Nl +
            shift + "}" + Nl


    type DictionaryUpdateType
        with
        member p.toFSharpCode = $"{p}"


    type ModelDataParams
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    modelInfo =" + Nl + (p.modelInfo.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    allParams =" + Nl +
            shift + "        [|"+ Nl +
            (toParamFSharpCode p.allParams (increaseShiftTwice shift |> increaseShift)) + Nl +
            shift + "        |]" + Nl +
            shift + "    collisionData =" + Nl + (p.collisionData.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    dictionaryUpdateType = " + (p.dictionaryUpdateType.toFSharpCode) + Nl +
            shift + "}" + Nl


    let toReactionsCode (a : list<ReactionName * int64>) (shift : string) =
        a
        |> List.map (fun (n, c) -> shift + "(" + n.ToString() + ", " + c.toFSharpCode + ")")
        |> String.concat Nl


    type AllSubstData
        with
        member p.toFSharpCode (shift : string) =
            let newShift = (increaseShiftTwice shift)

            shift + "{" + Nl +
            shift + "    allSubst = " + "allSubst" + Nl +
            shift + "    allInd = " + "allInd" + Nl +
            shift + "    allRawReactions =" + Nl +
            newShift + "[" + Nl +
            (toReactionsCode p.allRawReactions (increaseShift newShift)) + Nl +
            newShift + "]" + Nl +
            shift + "    allReactions =" + Nl +
            newShift + "[" + Nl +
            (toReactionsCode p.allReactions (increaseShift newShift)) + Nl +
            newShift + "]" + Nl +
            shift + "}" + Nl


    type ModelDataRegularParams
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    modelDataParams = " + Nl + (p.modelDataParams.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    allSubstData = " + Nl + (p.allSubstData.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "}" + Nl


    type ModelDataFuncParams
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    getTotals = " + "getTotals" + Nl +
            shift + "    getTotalSubst = " + "getTotalSubst" + Nl +
            shift + "    getDerivative = " + "update" + Nl +
            shift + "}" + Nl

    type ModelDataParamsWithExtraData
        with
        member p.toFSharpCode (shift : string) =
            shift + "{" + Nl +
            shift + "    regularParams = " + Nl + (p.regularParams.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "    funcParams = " + Nl + (p.funcParams.toFSharpCode (increaseShiftTwice shift)) + Nl +
            shift + "}" + Nl
