namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModels.SugarSynthesisModel
open ClmImpure.ReactionRateModels.SugarSynthesisRandomModel

module SugarSynthesisRandomModelExt =

    type SugarSynthesisRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SugarSynthesisRateModel (SugarSynthRndModel d) -> Some d
            | _ -> None


        static member tryCreate (p, m) =
            tryCreateModel SugarSynthesisRandomParam.paramGetter (fun d -> d |> SugarSynthesisRandomModel |> SugarSynthRndModel |> SugarSynthesisRateModel) (p, m)
