namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.SedimentationAllRandomModel
open ClmImpure.ReactionRateModels.SedimentationAllModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase

module SedimentationAllRandomModelExt =

    type SedimentationAllRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SedimentationAllRateModel d -> Some d
            | _ -> None


        static member tryCreate (p, m) =
            tryCreateModel SedimentationAllRandomParam.paramGetter (fun d -> d |> SedimentationAllRandomModel |> SedAllRndModel |> SedimentationAllRateModel) (p, m)
