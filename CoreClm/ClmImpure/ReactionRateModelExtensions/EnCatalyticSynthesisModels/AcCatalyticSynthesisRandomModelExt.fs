namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.SynthesisModel
open ClmImpure.ReactionRateModels.ActivationModel
open ClmImpure.ReactionRateModels.AcCatalyticSynthesisRandomModel
open ClmImpure.ReactionRateModels.AcCatalyticSynthesisModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.SynthesisModelExt
open ClmImpure.ReactionRateModelExtensions.ActivationModelExt

module AcCatalyticSynthesisRandomModelExt =

    type AcCatalyticSynthesisRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcCatalyticSynthesisRateModel (AcCatSynthRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d ac =
                { synthesisModel = b; acCatSynthRndParam = d; activationModel = ac }
                |> AcCatalyticSynthesisRandomModel
                |> AcCatSynthRndModel
                |> AcCatalyticSynthesisRateModel

            let x = (AcCatalyticSynthesisRandomParam.paramGetter, creator)
            let y = (SynthesisModel.modelGetter, SynthesisModel.tryCreate)
            let z = (ActivationModel.modelGetter, ActivationModel.tryCreate)

            tryCreateAcModelWithBase x y z (p, m)
