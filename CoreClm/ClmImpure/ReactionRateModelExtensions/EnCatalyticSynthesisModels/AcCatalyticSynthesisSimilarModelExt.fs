namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRates
open Clm.ReactionRateParams
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.AcCatalyticSynthesisSimilarModel
open ClmImpure.ReactionRateModels.AcCatalyticSynthesisRandomModel
open ClmImpure.ReactionRateModels.AcCatalyticSynthesisModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.AcCatalyticSynthesisRandomModelExt

module AcCatalyticSynthesisSimilarModelExt =

    type AcCatalyticSynthesisSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | AcCatalyticSynthesisRateParam (AcCatSynthSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcCatalyticSynthesisRateModel (AcCatSynthSimModel d) -> Some d
            | _ -> None


        static member tryCreate a (p, m) =
            let creator b (d : AcCatalyticSynthesisSimilarParam) =
                { acCatSynthModel = b; aminoAcids = a; acCatSynthSimParam = d.acCatSynthSimParam } |> AcCatalyticSynthesisSimilarModel |> AcCatSynthSimModel |> AcCatalyticSynthesisRateModel
            tryCreateModelWithBase AcCatalyticSynthesisSimilarModel.paramGetter creator AcCatalyticSynthesisRandomModel.modelGetter AcCatalyticSynthesisRandomModel.tryCreate (p, m)
