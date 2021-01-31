namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRates
open Clm.ReactionRateParams
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.AcBkwCatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.AcBkwCatalyticLigationSimilarModel
open ClmImpure.ReactionRateModels.AcBkwCatalyticLigationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.AcBkwCatalyticLigationRandomModelExt

module AcBkwCatalyticLigationSimilarModelExt =

    type AcBkwCatalyticLigationSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | AcBkwCatalyticLigationRateParam (AcBkwCatLigSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcBkwCatalyticLigationRateModel (AcBkwCatLigSimModel d) -> Some d
            | _ -> None


        static member tryCreate u a (p, m) =
            let creator b (d : AcBkwCatalyticLigationSimilarParam) =
                {
                    acBkwCatLigModel = b
                    peptideBondData = a
                    acBkwCatLigSimParam = d.acBkwCatLigSimParam
                    dictionaryUpdateType = u
                }
                |> AcBkwCatalyticLigationSimilarModel |> AcBkwCatLigSimModel |> AcBkwCatalyticLigationRateModel

            tryCreateModelWithBase AcBkwCatalyticLigationSimilarModel.paramGetter creator AcBkwCatalyticLigationRandomModel.modelGetter AcBkwCatalyticLigationRandomModel.tryCreate (p, m)
