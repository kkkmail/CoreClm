namespace ClmImpure.ReactionRateModelExtensions

open Clm.CalculationData
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.EnCatalyticRacemizationRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticRacemizationSimilarModel
open ClmImpure.ReactionRateModels.EnCatalyticRacemizationModel
open ClmImpure.ReactionRateModelExtensions.EnCatalyticRacemizationRandomModelExt
open ClmImpure.ReactionRateModelExtensions.EnCatalyticRacemizationSimilarModelExt

module EnCatalyticRacemizationModelExt =

    type EnCatalyticRacemizationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | EnCatalyticRacemizationRateModel d -> Some d
            | _ -> None

        static member tryCreate (si : SubstInfo) (p, m) =
            (p, m)
            |> EnCatalyticRacemizationRandomModel.tryCreate
            |> EnCatalyticRacemizationSimilarModel.tryCreate si.aminoAcids
