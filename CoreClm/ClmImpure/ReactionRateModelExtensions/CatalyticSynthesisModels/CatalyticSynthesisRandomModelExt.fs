namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.SynthesisModel
open ClmImpure.ReactionRateModels.CatalyticSynthesisRandomModel
open ClmImpure.ReactionRateModels.CatalyticSynthesisModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.SynthesisModelExt

module CatalyticSynthesisRandomModelExt =

    type CatalyticSynthesisRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticSynthesisRateModel (CatSynthRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d = { synthesisModel = b; catSynthRndParam = d } |> CatalyticSynthesisRandomModel |> CatSynthRndModel |> CatalyticSynthesisRateModel
            tryCreateModelWithBase CatalyticSynthesisRandomParam.paramGetter creator SynthesisModel.modelGetter SynthesisModel.tryCreate (p, m)
