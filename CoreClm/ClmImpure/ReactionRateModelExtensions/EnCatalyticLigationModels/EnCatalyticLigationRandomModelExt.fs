namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.LigationModel
open ClmImpure.ReactionRateModels.EnCatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticLigationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.LigationModelExt

module EnCatalyticLigationRandomModelExt =

    type EnCatalyticLigationRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | EnCatalyticLigationRateModel (EnCatLigRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d = { ligationModel = b; enCatLigationParam = d } |> EnCatalyticLigationRandomModel |> EnCatLigRndModel |> EnCatalyticLigationRateModel
            tryCreateModelWithBase EnCatalyticLigationRandomParam.paramGetter creator LigationModel.modelGetter LigationModel.tryCreate (p, m)
