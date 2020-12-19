namespace ClmImpure.ReactionRateModelExtensions

open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.SedimentationAllModel
open ClmImpure.ReactionRateModels.SedimentationAllRandomModel
open ClmImpure.ReactionRateModelExtensions.SedimentationAllRandomModelExt

module SedimentationAllModelExt =

    type SedimentationAllModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SedimentationAllRateModel d -> Some d
            | _ -> None

        static member tryCreate (p, m) = (p, m) |> SedimentationAllRandomModel.tryCreate
