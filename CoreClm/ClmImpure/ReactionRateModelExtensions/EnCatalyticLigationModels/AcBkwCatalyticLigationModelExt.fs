namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionTypes
open Clm.CalculationData
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.AcBkwCatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.AcBkwCatalyticLigationSimilarModel
open ClmImpure.ReactionRateModels.AcBkwCatalyticLigationModel
open ClmImpure.ReactionRateModelExtensions.AcBkwCatalyticLigationRandomModelExt
open ClmImpure.ReactionRateModelExtensions.AcBkwCatalyticLigationSimilarModelExt

module AcBkwCatalyticLigationModelExt =

    type AcBkwCatalyticLigationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcBkwCatalyticLigationRateModel d -> Some d
            | _ -> None

        static member tryCreate u (si : SubstInfo) (p, m) =
            (p, m)
            |> AcBkwCatalyticLigationRandomModel.tryCreate
            |> AcBkwCatalyticLigationSimilarModel.tryCreate u (si.ligationReactions |> PeptideBondData.create)
