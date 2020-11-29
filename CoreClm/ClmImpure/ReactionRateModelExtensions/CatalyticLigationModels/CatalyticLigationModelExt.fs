namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionTypes
open Clm.CalculationData
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.CatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.CatalyticLigationSimilarModel
open ClmImpure.ReactionRateModels.CatalyticLigationModel
open ClmImpure.ReactionRateModelExtensions.CatalyticLigationRandomModelExt
open ClmImpure.ReactionRateModelExtensions.CatalyticLigationSimilarModelExt

module CatalyticLigationModelExt =

    type CatalyticLigationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticLigationRateModel d -> Some d
            | _ -> None

        static member tryCreate (si : SubstInfo) (p, m) =
            (p, m)
            |> CatalyticLigationRandomModel.tryCreate
            |> CatalyticLigationSimilarModel.tryCreate (si.ligationReactions |> PeptideBondData.create)
