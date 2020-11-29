namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRates
open Clm.ReactionRateParams
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.CatalyticRacemizationRandomModel
open ClmImpure.ReactionRateModels.CatalyticRacemizationSimilarModel
open ClmImpure.ReactionRateModels.CatalyticRacemizationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.CatalyticRacemizationRandomModelExt

module CatalyticRacemizationSimilarModelExt =

    type CatalyticRacemizationSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | CatalyticRacemizationRateParam (CatRacemSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticRacemizationRateModel (CatRacemSimModel d) -> Some d
            | _ -> None


        static member tryCreate a (p, m) =
            let creator b (d : CatalyticRacemizationSimilarParam) =
                { catRacemModel = b; aminoAcids = a; catRacemSimParam = d.catRacemSimParam } |> CatalyticRacemizationSimilarModel |> CatRacemSimModel |> CatalyticRacemizationRateModel

            tryCreateModelWithBase CatalyticRacemizationSimilarModel.paramGetter creator CatalyticRacemizationRandomModel.modelGetter (CatalyticRacemizationRandomModel.tryCreate a) (p, m)
