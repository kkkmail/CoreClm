namespace ClmImpure.ReactionRateModelExtensions

open Clm.CalculationData
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.SedimentationDirectRandomModel
open ClmImpure.ReactionRateModels.SedimentationDirectSimilarModel
open ClmImpure.ReactionRateModels.SedimentationDirectModel
open ClmImpure.ReactionRateModelExtensions.SedimentationDirectRandomModelExt
open ClmImpure.ReactionRateModelExtensions.SedimentationDirectSimilarModelExt

module SedimentationDirectModelExt =

    type SedimentationDirectModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SedimentationDirectRateModel d -> Some d
            | _ -> None

        static member tryCreate (si : SubstInfo) (p, m) =
            (p, m)
            |> SedimentationDirectRandomModel.tryCreate
            |> SedimentationDirectSimilarModel.tryCreate (si.aminoAcids, si.sedDirReagents)
