namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.DestructionModel
open ClmImpure.ReactionRateModels.EnCatalyticDestructionRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticDestructionModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.DestructionModelExt

module EnCatalyticDestructionRandomModelExt =

    type EnCatalyticDestructionRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | EnCatalyticDestructionRateModel (EnCatDestrRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d = { destructionModel = b; enCatDestrRndParam = d } |> EnCatalyticDestructionRandomModel |> EnCatDestrRndModel |> EnCatalyticDestructionRateModel
            tryCreateModelWithBase EnCatalyticDestructionRandomParam.paramGetter creator DestructionModel.modelGetter DestructionModel.tryCreate (p, m)
