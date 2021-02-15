namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.LigationModel
open ClmImpure.ReactionRateModels.ActivationModel
open ClmImpure.ReactionRateModels.AcFwdCatalyticLigationRandomModel
open ClmImpure.ReactionRateModels.AcFwdCatalyticLigationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.LigationModelExt
open ClmImpure.ReactionRateModelExtensions.ActivationModelExt

module AcFwdCatalyticLigationRandomModelExt =

    type AcFwdCatalyticLigationRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcFwdCatalyticLigationRateModel (AcFwdCatLigRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d ac =
                { ligationModel = b; acFwdCatLigationParam = d; activationModel = ac }
                |> AcFwdCatalyticLigationRandomModel
                |> AcFwdCatLigRndModel
                |> AcFwdCatalyticLigationRateModel

            let x = (AcFwdCatalyticLigationRandomParam.paramGetter, creator)
            let y = (LigationModel.modelGetter, LigationModel.tryCreate)
            let z = (ActivationModel.modelGetter, ActivationModel.tryCreate)

            tryCreateAcModelWithBase x y z (p, m)
