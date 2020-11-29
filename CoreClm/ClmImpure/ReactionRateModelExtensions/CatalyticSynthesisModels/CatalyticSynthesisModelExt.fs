namespace ClmImpure.ReactionRateModelExtensions

open Clm.CalculationData
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.CatalyticSynthesisRandomModel
open ClmImpure.ReactionRateModels.CatalyticSynthesisSimilarModel
open ClmImpure.ReactionRateModels.CatalyticSynthesisModel
open ClmImpure.ReactionRateModelExtensions.CatalyticSynthesisRandomModelExt
open ClmImpure.ReactionRateModelExtensions.CatalyticSynthesisSimilarModelExt

module CatalyticSynthesisModelExt =

    type CatalyticSynthesisModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticSynthesisRateModel d -> Some d
            | _ -> None

        static member tryCreate (si : SubstInfo) (p, m) =
            (p, m)
            |> CatalyticSynthesisRandomModel.tryCreate
            |> CatalyticSynthesisSimilarModel.tryCreate si.aminoAcids
