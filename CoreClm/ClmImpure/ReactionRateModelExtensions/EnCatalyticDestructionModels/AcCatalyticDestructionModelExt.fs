namespace ClmImpure.ReactionRateModelExtensions

open Clm.CalculationData
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.AcCatalyticDestructionRandomModel
open ClmImpure.ReactionRateModels.AcCatalyticDestructionSimilarModel
open ClmImpure.ReactionRateModels.AcCatalyticDestructionModel
open ClmImpure.ReactionRateModelExtensions.AcCatalyticDestructionRandomModelExt
open ClmImpure.ReactionRateModelExtensions.AcCatalyticDestructionSimilarModelExt

module AcCatalyticDestructionModelExt =

    type AcCatalyticDestructionModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcCatalyticDestructionRateModel d -> Some d
            | _ -> None

        static member tryCreate (si : SubstInfo) (p, m) =
            (p, m)
            |> AcCatalyticDestructionRandomModel.tryCreate
            |> AcCatalyticDestructionSimilarModel.tryCreate si.aminoAcids
