namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModels.SynthesisModel
open ClmImpure.ReactionRateModels.SynthesisRandomModel

module SynthesisRandomModelExt =

    type SynthesisRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SynthesisRateModel (SynthRndModel d) -> Some d
            | _ -> None


        static member tryCreate (p, m) =
            tryCreateModel SynthesisRandomParam.paramGetter (fun d -> d |> SynthesisRandomModel |> SynthRndModel |> SynthesisRateModel) (p, m)
