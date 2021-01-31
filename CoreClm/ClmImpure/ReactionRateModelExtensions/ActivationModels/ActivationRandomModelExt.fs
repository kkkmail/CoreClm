namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModels.ActivationModel
open ClmImpure.ReactionRateModels.ActivationRandomModel

module ActivationRandomModelExt =

    type ActivationRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | ActivationRateModel (ActivationRndModel d) -> Some d
            | _ -> None


        static member tryCreate (p, m) =
            tryCreateModel ActivationRandomParam.paramGetter (fun d -> d |> ActivationRandomModel |> ActivationRndModel |> ActivationRateModel) (p, m)
