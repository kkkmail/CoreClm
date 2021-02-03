namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.SynthesisModel
open ClmImpure.ReactionRateModels.AcCatalyticSynthesisRandomModel
open ClmImpure.ReactionRateModels.AcCatalyticSynthesisModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.SynthesisModelExt

module AcCatalyticSynthesisRandomModelExt =

    type AcCatalyticSynthesisRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcCatalyticSynthesisRateModel (AcCatSynthRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d =
                { synthesisModel = b; acCatSynthRndParam = d; activationModel = a }
                |> AcCatalyticSynthesisRandomModel
                |> AcCatSynthRndModel
                |> AcCatalyticSynthesisRateModel

            tryCreateModelWithBase AcCatalyticSynthesisRandomParam.paramGetter creator SynthesisModel.modelGetter SynthesisModel.tryCreate (p, m)
