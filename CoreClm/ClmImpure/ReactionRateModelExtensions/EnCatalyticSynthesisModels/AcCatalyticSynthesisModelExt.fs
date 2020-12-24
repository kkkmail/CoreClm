namespace ClmImpure.ReactionRateModelExtensions

open Clm.CalculationData
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.AcCatalyticSynthesisRandomModel
open ClmImpure.ReactionRateModels.AcCatalyticSynthesisSimilarModel
open ClmImpure.ReactionRateModels.AcCatalyticSynthesisModel
open ClmImpure.ReactionRateModelExtensions.AcCatalyticSynthesisRandomModelExt
open ClmImpure.ReactionRateModelExtensions.AcCatalyticSynthesisSimilarModelExt

module AcCatalyticSynthesisModelExt =

    type AcCatalyticSynthesisModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcCatalyticSynthesisRateModel d -> Some d
            | _ -> None

        static member tryCreate (si : SubstInfo) (p, m) =
            (p, m)
            |> AcCatalyticSynthesisRandomModel.tryCreate
            |> AcCatalyticSynthesisSimilarModel.tryCreate si.aminoAcids
