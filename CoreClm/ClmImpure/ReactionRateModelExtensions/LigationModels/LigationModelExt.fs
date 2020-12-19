namespace ClmImpure.ReactionRateModelExtensions

open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.LigationModel
open ClmImpure.ReactionRateModels.LigationRandomModel
open ClmImpure.ReactionRateModelExtensions.LigationRandomModelExt

module LigationModelExt =

    type LigationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | LigationRateModel d -> Some d
            | _ -> None


        static member tryCreate (p, m) =
            (p, m)
            |> LigationRandomModel.tryCreate
