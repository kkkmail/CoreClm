namespace ClmImpure.ReactionRateModelExtensions

open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.SugarSynthesisRandomModel
open ClmImpure.ReactionRateModels.SugarSynthesisModel
open ClmImpure.ReactionRateModelExtensions.SugarSynthesisRandomModelExt

module SugarSynthesisModelExt =

    type SugarSynthesisModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SugarSynthesisRateModel d -> Some d
            | _ -> None


        static member tryCreate (p, m) = (p, m) |> SugarSynthesisRandomModel.tryCreate
