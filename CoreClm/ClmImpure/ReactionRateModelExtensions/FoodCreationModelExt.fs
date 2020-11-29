namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.FoodCreationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase

module FoodCreationModelExt =

    type FoodCreationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | FoodCreationRateModel d -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            tryCreateModel FoodCreationParam.paramGetter (fun d -> d |> FoodCreationModel |> FoodCreationRateModel) (p, m)
