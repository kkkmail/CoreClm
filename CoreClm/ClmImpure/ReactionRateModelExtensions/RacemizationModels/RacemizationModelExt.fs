namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.RacemizationRandomModel
open ClmImpure.ReactionRateModels.RacemizationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase

module RacemizationModelExt =

    type RacemizationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | RacemizationRateModel d -> Some d
            | _ -> None


        static member tryCreate (p, m) =
            tryCreateModel RacemizationRandomParam.paramGetter (fun d -> d |> RacemizationRandomModel |> RacemRndModel |> RacemizationRateModel) (p, m)
