namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRates
open Clm.ReactionRateParams
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.AcFwdCatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.AcFwdCatalyticLigationSimilarModel
open ClmImpure.ReactionRateModels.AcFwdCatalyticLigationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.AcFwdCatalyticLigationRandomModelExt

module AcFwdCatalyticLigationSimilarModelExt =

    type AcFwdCatalyticLigationSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | AcFwdCatalyticLigationRateParam (AcFwdCatLigSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcFwdCatalyticLigationRateModel (AcFwdCatLigSimModel d) -> Some d
            | _ -> None


        static member tryCreate a (p, m) =
            let creator b (d : AcFwdCatalyticLigationSimilarParam) =
                {
                    acFwdCatLigModel = b
                    peptideBondData = a
                    acFwdCatLigSimParam = d.acFwdCatLigSimParam
                }
                |> AcFwdCatalyticLigationSimilarModel |> AcFwdCatLigSimModel |> AcFwdCatalyticLigationRateModel

            tryCreateModelWithBase AcFwdCatalyticLigationSimilarModel.paramGetter creator AcFwdCatalyticLigationRandomModel.modelGetter AcFwdCatalyticLigationRandomModel.tryCreate (p, m)
