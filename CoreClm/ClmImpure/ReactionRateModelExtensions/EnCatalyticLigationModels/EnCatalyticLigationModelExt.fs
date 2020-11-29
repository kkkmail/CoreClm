namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionTypes
open Clm.CalculationData
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.EnCatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticLigationSimilarModel
open ClmImpure.ReactionRateModels.EnCatalyticLigationModel
open ClmImpure.ReactionRateModelExtensions.EnCatalyticLigationRandomModelExt
open ClmImpure.ReactionRateModelExtensions.EnCatalyticLigationSimilarModelExt

module EnCatalyticLigationModelExt =

    type EnCatalyticLigationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | EnCatalyticLigationRateModel d -> Some d
            | _ -> None

        static member tryCreate (si : SubstInfo) (p, m) =
            (p, m)
            |> EnCatalyticLigationRandomModel.tryCreate
            |> EnCatalyticLigationSimilarModel.tryCreate (si.ligationReactions |> PeptideBondData.create)
