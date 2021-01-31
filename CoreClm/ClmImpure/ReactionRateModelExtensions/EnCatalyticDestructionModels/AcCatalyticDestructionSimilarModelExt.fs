namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRates
open Clm.ReactionRateParams
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.AcCatalyticDestructionSimilarModel
open ClmImpure.ReactionRateModels.AcCatalyticDestructionRandomModel
open ClmImpure.ReactionRateModels.AcCatalyticDestructionModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.AcCatalyticDestructionRandomModelExt

module AcCatalyticDestructionSimilarModelExt =

    type AcCatalyticDestructionSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | AcCatalyticDestructionRateParam (AcCatDestrSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcCatalyticDestructionRateModel (AcCatDestrSimModel d) -> Some d
            | _ -> None


        static member tryCreate a (p, m) =
            let creator b (d : AcCatalyticDestructionSimilarParam) =
                { acCatDestrModel = b; aminoAcids = a; acCatDestrSimParam = d.acCatDestrSimParam } |> AcCatalyticDestructionSimilarModel |> AcCatDestrSimModel |> AcCatalyticDestructionRateModel
            tryCreateModelWithBase AcCatalyticDestructionSimilarModel.paramGetter creator AcCatalyticDestructionRandomModel.modelGetter AcCatalyticDestructionRandomModel.tryCreate (p, m)
