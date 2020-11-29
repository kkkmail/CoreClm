namespace ClmImpure.ReactionRateModelExtensions

open Clm.CalculationData
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.CatalyticRacemizationRandomModel
open ClmImpure.ReactionRateModels.CatalyticRacemizationSimilarModel
open ClmImpure.ReactionRateModels.CatalyticRacemizationModel
open ClmImpure.ReactionRateModelExtensions.CatalyticRacemizationRandomModelExt
open ClmImpure.ReactionRateModelExtensions.CatalyticRacemizationSimilarModelExt

module CatalyticRacemizationModelExt =

    type CatalyticRacemizationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticRacemizationRateModel d -> Some d
            | _ -> None

        static member tryCreate (si : SubstInfo) (p, m) =
            (p, m)
            |> CatalyticRacemizationRandomModel.tryCreate si.aminoAcids
            |> CatalyticRacemizationSimilarModel.tryCreate si.aminoAcids
