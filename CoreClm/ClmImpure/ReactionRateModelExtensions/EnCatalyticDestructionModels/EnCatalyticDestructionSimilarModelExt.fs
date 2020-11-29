namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRates
open Clm.ReactionRateParams
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.EnCatalyticDestructionSimilarModel
open ClmImpure.ReactionRateModels.EnCatalyticDestructionRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticDestructionModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.EnCatalyticDestructionRandomModelExt

module EnCatalyticDestructionSimilarModelExt =

    type EnCatalyticDestructionSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | EnCatalyticDestructionRateParam (EnCatDestrSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | EnCatalyticDestructionRateModel (EnCatDestrSimModel d) -> Some d
            | _ -> None


        static member tryCreate a (p, m) =
            let creator b (d : EnCatalyticDestructionSimilarParam) =
                { enCatDestrModel = b; aminoAcids = a; enCatDestrSimParam = d.enCatDestrSimParam } |> EnCatalyticDestructionSimilarModel |> EnCatDestrSimModel |> EnCatalyticDestructionRateModel
            tryCreateModelWithBase EnCatalyticDestructionSimilarModel.paramGetter creator EnCatalyticDestructionRandomModel.modelGetter EnCatalyticDestructionRandomModel.tryCreate (p, m)
