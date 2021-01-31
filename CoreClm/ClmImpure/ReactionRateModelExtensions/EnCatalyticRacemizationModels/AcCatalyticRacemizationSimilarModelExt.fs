namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRates
open Clm.ReactionRateParams
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.AcCatalyticRacemizationSimilarModel
open ClmImpure.ReactionRateModels.AcCatalyticRacemizationRandomModel
open ClmImpure.ReactionRateModels.AcCatalyticRacemizationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.AcCatalyticRacemizationRandomModelExt

module AcCatalyticRacemizationSimilarModelExt =

    type AcCatalyticRacemizationSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | AcCatalyticRacemizationRateParam (AcCatRacemSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcCatalyticRacemizationRateModel (AcCatRacemSimModel d) -> Some d
            | _ -> None


        static member tryCreate a (p, m) =
            let creator b (d : AcCatalyticRacemizationSimilarParam) =
                { acCatRacemModel = b; aminoAcids = a; acCatRacemSimParam = d.acCatRacemSimParam } |> AcCatalyticRacemizationSimilarModel |> AcCatRacemSimModel |> AcCatalyticRacemizationRateModel
            tryCreateModelWithBase AcCatalyticRacemizationSimilarModel.paramGetter creator AcCatalyticRacemizationRandomModel.modelGetter AcCatalyticRacemizationRandomModel.tryCreate (p, m)
