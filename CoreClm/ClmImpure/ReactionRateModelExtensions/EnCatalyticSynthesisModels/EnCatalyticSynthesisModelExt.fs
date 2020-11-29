namespace ClmImpure.ReactionRateModelExtensions

open Clm.CalculationData
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.EnCatalyticSynthesisRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticSynthesisSimilarModel
open ClmImpure.ReactionRateModels.EnCatalyticSynthesisModel
open ClmImpure.ReactionRateModelExtensions.EnCatalyticSynthesisRandomModelExt
open ClmImpure.ReactionRateModelExtensions.EnCatalyticSynthesisSimilarModelExt

module EnCatalyticSynthesisModelExt =

    type EnCatalyticSynthesisModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | EnCatalyticSynthesisRateModel d -> Some d
            | _ -> None

        static member tryCreate (si : SubstInfo) (p, m) =
            (p, m)
            |> EnCatalyticSynthesisRandomModel.tryCreate
            |> EnCatalyticSynthesisSimilarModel.tryCreate si.aminoAcids
