namespace ClmImpure.ReactionRateModels

open ClmImpure.ReactionRateModels.AcBkwCatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.AcBkwCatalyticLigationSimilarModel

module AcBkwCatalyticLigationModel =

    type AcBkwCatalyticLigationParamWithModel =
        | AcBkwCatLigRndParamWithModel of AcBkwCatalyticLigationRandomParamWithModel
        | AcBkwCatLigSimParamWithModel of AcBkwCatalyticLigationSimilarParamWithModel


    type AcBkwCatalyticLigationModel =
        | AcBkwCatLigRndModel of AcBkwCatalyticLigationRandomModel
        | AcBkwCatLigSimModel of AcBkwCatalyticLigationSimilarModel

        member model.getRates t rnd r =
            match model with
            | AcBkwCatLigRndModel m -> m.getRates t rnd r
            | AcBkwCatLigSimModel m -> m.getRates t rnd r

        member model.inputParams =
            match model with
            | AcBkwCatLigRndModel m -> m.inputParams |> AcBkwCatLigRndParamWithModel
            | AcBkwCatLigSimModel m -> m.inputParams |> AcBkwCatLigSimParamWithModel

        member model.getAllRates() =
            match model with
            | AcBkwCatLigRndModel m -> m.getAllRates()
            | AcBkwCatLigSimModel m -> m.getAllRates()

        static member create p =
            match p with
            | AcBkwCatLigRndParamWithModel q -> AcBkwCatalyticLigationRandomModel q |> AcBkwCatLigRndModel
            | AcBkwCatLigSimParamWithModel q -> AcBkwCatalyticLigationSimilarModel q |> AcBkwCatLigSimModel
