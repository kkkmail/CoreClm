namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.LigationModel
open ClmImpure.ReactionRateModels.AcFwdCatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.AcFwdCatalyticLigationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.LigationModelExt

module AcFwdCatalyticLigationRandomModelExt =

    type AcFwdCatalyticLigationRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcFwdCatalyticLigationRateModel (AcFwdCatLigRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d = { ligationModel = b; acFwdCatLigationParam = d } |> AcFwdCatalyticLigationRandomModel |> AcFwdCatLigRndModel |> AcFwdCatalyticLigationRateModel
            tryCreateModelWithBase AcFwdCatalyticLigationRandomParam.paramGetter creator LigationModel.modelGetter LigationModel.tryCreate (p, m)
