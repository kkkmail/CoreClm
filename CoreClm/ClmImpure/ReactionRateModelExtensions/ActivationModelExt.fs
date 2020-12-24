namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.ActivationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase

module ActivationModelExt =

    type ActivationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | ActivationRateModel d -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            tryCreateModel ActivationParam.paramGetter (fun d -> d |> ActivationModel |> ActivationRateModel) (p, m)
