namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.LigationRandomModel
open ClmImpure.ReactionRateModels.LigationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase

module LigationRandomModelExt =

    type LigationRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | LigationRateModel (LigRndModel d) -> Some d
            | _ -> None


        static member tryCreate (p, m) =
            tryCreateModel LigationRandomParam.paramGetter (fun d -> d |> LigationRandomModel |> LigRndModel |> LigationRateModel) (p, m)
