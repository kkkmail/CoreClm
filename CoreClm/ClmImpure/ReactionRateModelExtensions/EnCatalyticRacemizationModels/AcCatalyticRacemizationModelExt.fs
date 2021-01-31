namespace ClmImpure.ReactionRateModelExtensions

open Clm.CalculationData
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.AcCatalyticRacemizationRandomModel
open ClmImpure.ReactionRateModels.AcCatalyticRacemizationSimilarModel
open ClmImpure.ReactionRateModels.AcCatalyticRacemizationModel
open ClmImpure.ReactionRateModelExtensions.AcCatalyticRacemizationRandomModelExt
open ClmImpure.ReactionRateModelExtensions.AcCatalyticRacemizationSimilarModelExt

module AcCatalyticRacemizationModelExt =

    type AcCatalyticRacemizationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcCatalyticRacemizationRateModel d -> Some d
            | _ -> None

        static member tryCreate (si : SubstInfo) (p, m) =
            (p, m)
            |> AcCatalyticRacemizationRandomModel.tryCreate
            |> AcCatalyticRacemizationSimilarModel.tryCreate si.aminoAcids
