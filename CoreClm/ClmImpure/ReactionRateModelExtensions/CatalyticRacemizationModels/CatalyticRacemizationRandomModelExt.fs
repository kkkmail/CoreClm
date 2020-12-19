namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.RacemizationModel
open ClmImpure.ReactionRateModels.CatalyticRacemizationRandomModel
open ClmImpure.ReactionRateModels.CatalyticRacemizationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.RacemizationModelExt

module CatalyticRacemizationRandomModelExt =

    type CatalyticRacemizationRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticRacemizationRateModel (CatRacemRndModel d) -> Some d
            | _ -> None

        static member tryCreate a (p, m) =
            let creator b d = { racemizationModel = b; aminoAcids = a; catRacemRndParam = d } |> CatalyticRacemizationRandomModel |> CatRacemRndModel |> CatalyticRacemizationRateModel
            tryCreateModelWithBase CatalyticRacemizationRandomParam.paramGetter creator RacemizationModel.modelGetter RacemizationModel.tryCreate (p, m)
