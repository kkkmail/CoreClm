namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.DestructionModel
open ClmImpure.ReactionRateModels.CatalyticDestructionRandomModel
open ClmImpure.ReactionRateModels.CatalyticDestructionModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.DestructionModelExt

module CatalyticDestructionRandomModelExt =

    type CatalyticDestructionRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticDestructionRateModel (CatDestrRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d = { destructionModel = b; catDestrRndParam = d } |> CatalyticDestructionRandomModel |> CatDestrRndModel |> CatalyticDestructionRateModel
            tryCreateModelWithBase CatalyticDestructionRandomParam.paramGetter creator DestructionModel.modelGetter DestructionModel.tryCreate (p, m)
