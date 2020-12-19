namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.SedimentationDirectRandomModel
open ClmImpure.ReactionRateModels.SedimentationDirectModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase

module SedimentationDirectRandomModelExt =

    type SedimentationDirectRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SedimentationDirectRateModel (SedDirRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            tryCreateModel SedimentationDirectRandomParam.paramGetter (fun d -> d |> SedimentationDirectRandomModel |> SedDirRndModel |> SedimentationDirectRateModel) (p, m)
