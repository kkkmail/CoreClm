namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.RacemizationModel
open ClmImpure.ReactionRateModels.AcCatalyticRacemizationRandomModel
open ClmImpure.ReactionRateModels.AcCatalyticRacemizationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.RacemizationModelExt

module AcCatalyticRacemizationRandomModelExt =

    type AcCatalyticRacemizationRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcCatalyticRacemizationRateModel (AcCatRacemRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d = { racemizationModel = b; acCatRacemRndParam = d } |> AcCatalyticRacemizationRandomModel |> AcCatRacemRndModel |> AcCatalyticRacemizationRateModel
            tryCreateModelWithBase AcCatalyticRacemizationRandomParam.paramGetter creator RacemizationModel.modelGetter RacemizationModel.tryCreate (p, m)
