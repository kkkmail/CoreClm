namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.SynthesisModel
open ClmImpure.ReactionRateModels.EnCatalyticSynthesisRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticSynthesisModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.SynthesisModelExt

module EnCatalyticSynthesisRandomModelExt =

    type EnCatalyticSynthesisRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | EnCatalyticSynthesisRateModel (EnCatSynthRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d = { synthesisModel = b; enCatSynthRndParam = d } |> EnCatalyticSynthesisRandomModel |> EnCatSynthRndModel |> EnCatalyticSynthesisRateModel
            tryCreateModelWithBase EnCatalyticSynthesisRandomParam.paramGetter creator SynthesisModel.modelGetter SynthesisModel.tryCreate (p, m)
