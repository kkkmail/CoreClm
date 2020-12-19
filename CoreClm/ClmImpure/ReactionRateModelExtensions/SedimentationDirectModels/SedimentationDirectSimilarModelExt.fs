namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRates
open Clm.ReactionRateParams
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.SedimentationDirectRandomModel
open ClmImpure.ReactionRateModels.SedimentationDirectSimilarModel
open ClmImpure.ReactionRateModels.SedimentationDirectModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.SedimentationDirectRandomModelExt

module SedimentationDirectSimilarModelExt =

    type SedimentationDirectSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | SedimentationDirectRateParam (SedDirSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SedimentationDirectRateModel (SedDirSimModel d) -> Some d
            | _ -> None


        static member tryCreate (a, r) (p, m) =
            let creator b (d : SedimentationDirectSimilarParam) = { sedDirModel = b; aminoAcids = a; sedDirSimParam = d.sedDirSimParam; reagents = r } |> SedimentationDirectSimilarModel |> SedDirSimModel |> SedimentationDirectRateModel
            tryCreateModelWithBase SedimentationDirectSimilarModel.paramGetter creator SedimentationDirectRandomModel.modelGetter SedimentationDirectRandomModel.tryCreate (p, m)
