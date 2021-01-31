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

        member model.getRates t rnd r =
            match model with
            | AcFwdCatLigRndModel m -> m.getRates t rnd r
            | AcFwdCatLigSimModel m -> m.getRates t rnd r

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
