namespace ClmImpure.ReactionRateModelExtensions

open Clm.CalculationData
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.CatalyticDestructionModel
open ClmImpure.ReactionRateModels.CatalyticDestructionRandomModel
open ClmImpure.ReactionRateModels.CatalyticDestructionSimilarModel
open ClmImpure.ReactionRateModelExtensions.CatalyticDestructionRandomModelExt
open ClmImpure.ReactionRateModelExtensions.CatalyticDestructionSimilarModelExt

module CatalyticDestructionModelExt =

    type CatalyticDestructionModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticDestructionRateModel d -> Some d
            | _ -> None

        static member tryCreate (si : SubstInfo) (p, m) =
            (p, m)
            |> CatalyticDestructionRandomModel.tryCreate
            |> CatalyticDestructionSimilarModel.tryCreate si.aminoAcids
