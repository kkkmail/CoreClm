namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRates
open Clm.ReactionRateParams
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.EnCatalyticRacemizationSimilarModel
open ClmImpure.ReactionRateModels.EnCatalyticRacemizationRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticRacemizationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.EnCatalyticRacemizationRandomModelExt

module EnCatalyticRacemizationSimilarModelExt =

    type EnCatalyticRacemizationSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | EnCatalyticRacemizationRateParam (EnCatRacemSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | EnCatalyticRacemizationRateModel (EnCatRacemSimModel d) -> Some d
            | _ -> None


        static member tryCreate a (p, m) =
            let creator b (d : EnCatalyticRacemizationSimilarParam) =
                { enCatRacemModel = b; aminoAcids = a; enCatRacemSimParam = d.enCatRacemSimParam } |> EnCatalyticRacemizationSimilarModel |> EnCatRacemSimModel |> EnCatalyticRacemizationRateModel
            tryCreateModelWithBase EnCatalyticRacemizationSimilarModel.paramGetter creator EnCatalyticRacemizationRandomModel.modelGetter EnCatalyticRacemizationRandomModel.tryCreate (p, m)
