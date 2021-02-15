namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.DestructionModel
open ClmImpure.ReactionRateModels.ActivationModel
open ClmImpure.ReactionRateModels.AcCatalyticDestructionRandomModel
open ClmImpure.ReactionRateModels.AcCatalyticDestructionModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.DestructionModelExt
open ClmImpure.ReactionRateModelExtensions.ActivationModelExt

module AcCatalyticDestructionRandomModelExt =

    type AcCatalyticDestructionRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcCatalyticDestructionRateModel (AcCatDestrRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d ac =
                { destructionModel = b; acCatDestrRndParam = d; activationModel = ac }
                |> AcCatalyticDestructionRandomModel
                |> AcCatDestrRndModel
                |> AcCatalyticDestructionRateModel

            let x = (AcCatalyticDestructionRandomParam.paramGetter, creator)
            let y = (DestructionModel.modelGetter, DestructionModel.tryCreate)
            let z = (ActivationModel.modelGetter, ActivationModel.tryCreate)

            tryCreateAcModelWithBase x y z (p, m)
