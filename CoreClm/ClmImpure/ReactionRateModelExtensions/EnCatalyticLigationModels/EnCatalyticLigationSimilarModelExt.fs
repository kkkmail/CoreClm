namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRates
open Clm.ReactionRateParams
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.EnCatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticLigationSimilarModel
open ClmImpure.ReactionRateModels.EnCatalyticLigationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.EnCatalyticLigationRandomModelExt

module EnCatalyticLigationSimilarModelExt =

    type EnCatalyticLigationSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | EnCatalyticLigationRateParam (EnCatLigSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | EnCatalyticLigationRateModel (EnCatLigSimModel d) -> Some d
            | _ -> None


        static member tryCreate a (p, m) =
            let creator b (d : EnCatalyticLigationSimilarParam) =
                {
                    enCatLigModel = b
                    peptideBondData = a
                    enCatLigSimParam = d.enCatLigSimParam
                }
                |> EnCatalyticLigationSimilarModel |> EnCatLigSimModel |> EnCatalyticLigationRateModel

            tryCreateModelWithBase EnCatalyticLigationSimilarModel.paramGetter creator EnCatalyticLigationRandomModel.modelGetter EnCatalyticLigationRandomModel.tryCreate (p, m)
