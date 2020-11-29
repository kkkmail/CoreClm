namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.RacemizationModel
open ClmImpure.ReactionRateModels.EnCatalyticRacemizationRandomModel
open ClmImpure.ReactionRateModels.EnCatalyticRacemizationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.RacemizationModelExt

module EnCatalyticRacemizationRandomModelExt =

    type EnCatalyticRacemizationRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | EnCatalyticRacemizationRateModel (EnCatRacemRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d = { racemizationModel = b; enCatRacemRndParam = d } |> EnCatalyticRacemizationRandomModel |> EnCatRacemRndModel |> EnCatalyticRacemizationRateModel
            tryCreateModelWithBase EnCatalyticRacemizationRandomParam.paramGetter creator RacemizationModel.modelGetter RacemizationModel.tryCreate (p, m)
