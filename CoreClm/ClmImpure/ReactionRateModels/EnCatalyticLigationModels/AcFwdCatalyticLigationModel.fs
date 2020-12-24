namespace ClmImpure.ReactionRateModels

open ClmImpure.ReactionRateModels.AcFwdCatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.AcFwdCatalyticLigationSimilarModel

module AcFwdCatalyticLigationModel =

    type AcFwdCatalyticLigationParamWithModel =
        | AcFwdCatLigRndParamWithModel of AcFwdCatalyticLigationRandomParamWithModel
        | AcFwdCatLigSimParamWithModel of AcFwdCatalyticLigationSimilarParamWithModel


    type AcFwdCatalyticLigationModel =
        | AcFwdCatLigRndModel of AcFwdCatalyticLigationRandomModel
        | AcFwdCatLigSimModel of AcFwdCatalyticLigationSimilarModel

        member model.getRates rnd t r =
            match model with
            | AcFwdCatLigRndModel m -> m.getRates rnd t r
            | AcFwdCatLigSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | AcFwdCatLigRndModel m -> m.inputParams |> AcFwdCatLigRndParamWithModel
            | AcFwdCatLigSimModel m -> m.inputParams |> AcFwdCatLigSimParamWithModel

        member model.getAllRates() =
            match model with
            | AcFwdCatLigRndModel m -> m.getAllRates()
            | AcFwdCatLigSimModel m -> m.getAllRates()

        static member create p =
            match p with
            | AcFwdCatLigRndParamWithModel q -> AcFwdCatalyticLigationRandomModel q |> AcFwdCatLigRndModel
            | AcFwdCatLigSimParamWithModel q -> AcFwdCatalyticLigationSimilarModel q |> AcFwdCatLigSimModel
