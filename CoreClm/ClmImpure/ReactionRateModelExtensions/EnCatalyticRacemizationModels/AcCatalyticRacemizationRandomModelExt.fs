namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRateParams
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.RacemizationModel
open ClmImpure.ReactionRateModels.ActivationModel
open ClmImpure.ReactionRateModels.AcCatalyticRacemizationRandomModel
open ClmImpure.ReactionRateModels.AcCatalyticRacemizationModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase
open ClmImpure.ReactionRateModelExtensions.RacemizationModelExt
open ClmImpure.ReactionRateModelExtensions.ActivationModelExt

module AcCatalyticRacemizationRandomModelExt =

    type AcCatalyticRacemizationRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | AcCatalyticRacemizationRateModel (AcCatRacemRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d ac =
                { racemizationModel = b; acCatRacemRndParam = d; activationModel = ac }
                |> AcCatalyticRacemizationRandomModel
                |> AcCatRacemRndModel
                |> AcCatalyticRacemizationRateModel

            let x = (AcCatalyticRacemizationRandomParam.paramGetter, creator)
            let y = (RacemizationModel.modelGetter, RacemizationModel.tryCreate)
            let z = (ActivationModel.modelGetter, ActivationModel.tryCreate)

            tryCreateAcModelWithBase x y z (p, m)
