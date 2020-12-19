namespace ClmImpure.ReactionRateModels

open Clm.ReactionRateParams
open ClmImpure.ReactionRateModels.SedimentationDirectRandomModel
open ClmImpure.ReactionRateModels.SedimentationDirectSimilarModel

module SedimentationDirectModel =

    type SedimentationDirectParamWithModel =
        | SedDirRndParamWithModel of SedimentationDirectRandomParam
        | SedDiSimParamWithModel of SedimentationDirectSimilarParamWithModel


    type SedimentationDirectModel =
        | SedDirRndModel of SedimentationDirectRandomModel
        | SedDirSimModel of SedimentationDirectSimilarModel

        member model.getRates rnd t r =
            match model with
            | SedDirRndModel m -> m.getRates rnd t r
            | SedDirSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | SedDirRndModel m -> m.inputParams |> SedDirRndParamWithModel
            | SedDirSimModel m -> m.inputParams |> SedDiSimParamWithModel

        member model.getAllRates() =
            match model with
            | SedDirRndModel m -> m.getAllRates()
            | SedDirSimModel m -> m.getAllRates()

        static member create p =
            match p with
            | SedDirRndParamWithModel q -> SedimentationDirectRandomModel q |> SedDirRndModel
            | SedDiSimParamWithModel q -> SedimentationDirectSimilarModel q |> SedDirSimModel
