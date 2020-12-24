namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.DestructionModel
open ClmImpure.ReactionRateModels.AcCatalyticDestructionRandomModel
open ClmImpure.ReactionRateModels.AcCatalyticDestructionModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.DestructionModelExt

module AcCatalyticDestructionRandomModelExt =

    type AcCatalyticDestructionRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcCatalyticDestructionRateModel (AcCatDestrRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d = { destructionModel = b; acCatDestrRndParam = d } |> AcCatalyticDestructionRandomModel |> AcCatDestrRndModel |> AcCatalyticDestructionRateModel
            tryCreateModelWithBase AcCatalyticDestructionRandomParam.paramGetter creator DestructionModel.modelGetter DestructionModel.tryCreate (p, m)
