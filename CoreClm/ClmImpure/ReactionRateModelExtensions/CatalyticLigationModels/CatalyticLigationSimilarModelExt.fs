namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRates
open Clm.ReactionRateParams
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.CatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.CatalyticLigationSimilarModel
open ClmImpure.ReactionRateModels.CatalyticLigationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.CatalyticLigationRandomModelExt

module CatalyticLigationSimilarModelExt =

    type CatalyticLigationSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | CatalyticLigationRateParam (CatLigSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticLigationRateModel (CatLigSimModel d) -> Some d
            | _ -> None


        static member tryCreate a (p, m) =
            let creator b (d : CatalyticLigationSimilarParam) =
                {
                    catLigModel = b
                    peptideBondData = a
                    catLigSimParam = d.catLigSimParam
                }
                |> CatalyticLigationSimilarModel |> CatLigSimModel |> CatalyticLigationRateModel

            tryCreateModelWithBase CatalyticLigationSimilarModel.paramGetter creator CatalyticLigationRandomModel.modelGetter CatalyticLigationRandomModel.tryCreate (p, m)
