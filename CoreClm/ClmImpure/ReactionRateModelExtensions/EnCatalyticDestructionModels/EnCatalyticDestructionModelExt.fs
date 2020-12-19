namespace ClmImpure.ReactionRateModelExtensions

open Clm.CalculationData
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.EnCatalyticDestructionRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticDestructionSimilarModel
open ClmImpure.ReactionRateModels.EnCatalyticDestructionModel
open ClmImpure.ReactionRateModelExtensions.EnCatalyticDestructionRandomModelExt
open ClmImpure.ReactionRateModelExtensions.EnCatalyticDestructionSimilarModelExt

module EnCatalyticDestructionModelExt =

    type EnCatalyticDestructionModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | EnCatalyticDestructionRateModel d -> Some d
            | _ -> None

        static member tryCreate (si : SubstInfo) (p, m) =
            (p, m)
            |> EnCatalyticDestructionRandomModel.tryCreate
            |> EnCatalyticDestructionSimilarModel.tryCreate si.aminoAcids
