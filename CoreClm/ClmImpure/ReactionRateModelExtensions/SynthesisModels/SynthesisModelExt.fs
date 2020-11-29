namespace ClmImpure.ReactionRateModelExtensions

open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.SynthesisModel
open ClmImpure.ReactionRateModels.SynthesisRandomModel
open ClmImpure.ReactionRateModelExtensions.SynthesisRandomModelExt

module SynthesisModelExt =
    type SynthesisModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SynthesisRateModel d -> Some d
            | _ -> None


        static member tryCreate (p, m) = (p, m) |> SynthesisRandomModel.tryCreate
