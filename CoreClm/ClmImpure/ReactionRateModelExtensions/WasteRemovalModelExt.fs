namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.WasteRemovalModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase

module WasteRemovalModelExt =

    type WasteRemovalModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | WasteRemovalRateModel d -> Some d
            | _ -> None


        static member tryCreate (p, m) =
            tryCreateModel WasteRemovalParam.paramGetter (fun d -> d |> WasteRemovalModel |> WasteRemovalRateModel) (p, m)
