namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.LigationModel
open ClmImpure.ReactionRateModels.AcBkwCatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.AcBkwCatalyticLigationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.LigationModelExt

module AcBkwCatalyticLigationRandomModelExt =

    type AcBkwCatalyticLigationRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcBkwCatalyticLigationRateModel (AcBkwCatLigRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d = { ligationModel = b; acBkwCatLigationParam = d } |> AcBkwCatalyticLigationRandomModel |> AcBkwCatLigRndModel |> AcBkwCatalyticLigationRateModel
            tryCreateModelWithBase AcBkwCatalyticLigationRandomParam.paramGetter creator LigationModel.modelGetter LigationModel.tryCreate (p, m)
