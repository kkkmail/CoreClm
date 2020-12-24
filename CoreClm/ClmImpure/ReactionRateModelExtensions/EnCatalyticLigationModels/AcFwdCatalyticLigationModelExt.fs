namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionTypes
open Clm.CalculationData
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.AcFwdCatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.AcFwdCatalyticLigationSimilarModel
open ClmImpure.ReactionRateModels.AcFwdCatalyticLigationModel
open ClmImpure.ReactionRateModelExtensions.AcFwdCatalyticLigationRandomModelExt
open ClmImpure.ReactionRateModelExtensions.AcFwdCatalyticLigationSimilarModelExt

module AcFwdCatalyticLigationModelExt =

    type AcFwdCatalyticLigationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcFwdCatalyticLigationRateModel d -> Some d
            | _ -> None

        static member tryCreate (si : SubstInfo) (p, m) =
            (p, m)
            |> AcFwdCatalyticLigationRandomModel.tryCreate
            |> AcFwdCatalyticLigationSimilarModel.tryCreate (si.ligationReactions |> PeptideBondData.create)
