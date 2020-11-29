namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRates
open Clm.ReactionRateParams
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.CatalyticSynthesisSimilarModel
open ClmImpure.ReactionRateModels.CatalyticSynthesisRandomModel
open ClmImpure.ReactionRateModels.CatalyticSynthesisModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.CatalyticSynthesisRandomModelExt

module CatalyticSynthesisSimilarModelExt =

    type CatalyticSynthesisSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | CatalyticSynthesisRateParam (CatSynthSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticSynthesisRateModel (CatSynthSimModel d) -> Some d
            | _ -> None


        static member tryCreate a (p, m) =
            let creator b (d : CatalyticSynthesisSimilarParam) = { catSynthModel = b; aminoAcids = a; catSynthSimParam = d.catSynthSimParam } |> CatalyticSynthesisSimilarModel |> CatSynthSimModel |> CatalyticSynthesisRateModel
            tryCreateModelWithBase CatalyticSynthesisSimilarModel.paramGetter creator CatalyticSynthesisRandomModel.modelGetter CatalyticSynthesisRandomModel.tryCreate (p, m)
