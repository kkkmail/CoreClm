namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.WasteRecyclingModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase

module WasteRecyclingModelExt =

    type WasteRecyclingModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | WasteRecyclingRateModel d -> Some d
            | _ -> None


        static member tryCreate (p, m) =
            tryCreateModel WasteRecyclingParam.paramGetter (fun d -> d |> WasteRecyclingModel |> WasteRecyclingRateModel) (p, m)
