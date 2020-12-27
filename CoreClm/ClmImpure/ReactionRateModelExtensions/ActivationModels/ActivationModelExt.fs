namespace ClmImpure.ReactionRateModelExtensions

open Clm.CalculationData
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.ActivationRandomModel
open ClmImpure.ReactionRateModels.ActivationModel
open ClmImpure.ReactionRateModelExtensions.ActivationRandomModelExt

module ActivationModelExt =

    type ActivationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | ActivationRateModel d -> Some d
            | _ -> None

        static member tryCreate (si : SubstInfo) (p, m) =
            (p, m)
            |> ActivationRandomModel.tryCreate
