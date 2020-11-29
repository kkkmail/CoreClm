namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRates
open Clm.ReactionRateParams
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.CatalyticDestructionRandomModel
open ClmImpure.ReactionRateModels.CatalyticDestructionSimilarModel
open ClmImpure.ReactionRateModels.CatalyticDestructionModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.CatalyticDestructionRandomModelExt

module CatalyticDestructionSimilarModelExt =

    type CatalyticDestructionSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | CatalyticDestructionRateParam (CatDestrSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticDestructionRateModel (CatDestrSimModel d) -> Some d
            | _ -> None


        static member tryCreate a (p, m) =
            let creator b (d : CatalyticDestructionSimilarParam) =
                { catDestrModel = b; aminoAcids = a; catDestrSimParam = d.catDestrSimParam } |> CatalyticDestructionSimilarModel |> CatDestrSimModel |> CatalyticDestructionRateModel

            tryCreateModelWithBase CatalyticDestructionSimilarModel.paramGetter creator CatalyticDestructionRandomModel.modelGetter CatalyticDestructionRandomModel.tryCreate (p, m)
