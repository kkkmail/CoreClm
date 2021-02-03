namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.LigationModel
open ClmImpure.ReactionRateModels.ActivationModel
open ClmImpure.ReactionRateModels.AcBkwCatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.AcBkwCatalyticLigationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.LigationModelExt
open ClmImpure.ReactionRateModelExtensions.ActivationModelExt

module AcBkwCatalyticLigationRandomModelExt =

    type AcBkwCatalyticLigationRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcBkwCatalyticLigationRateModel (AcBkwCatLigRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d ac =
                { ligationModel = b; acBkwCatLigationParam = d; activationModel = ac }
                |> AcBkwCatalyticLigationRandomModel
                |> AcBkwCatLigRndModel
                |> AcBkwCatalyticLigationRateModel

            let x = (AcBkwCatalyticLigationRandomParam.paramGetter, creator)
            let y = (LigationModel.modelGetter, LigationModel.tryCreate)
            let z = (ActivationModel.modelGetter, ActivationModel.tryCreate)

            tryCreateAcModelWithBase x y z (p, m)
