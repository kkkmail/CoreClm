namespace ClmDefaults

open Clm.Distributions
open Clm.ReactionRates
open Clm.ReactionRatesBase
open Clm.ReactionRateParams

module DefaultValuesExt =

    /// Converts a list of 'A into a list of tuples (i, 'A) where i is a zero based int64 element number in the original list.
    let withRowNumber a = a |> List.mapi (fun i e -> (int64 i, e))


    let defaultRateMultiplierDistr threshold mult =
        Distribution.createTriangular { threshold = threshold; scale = Some mult; shift = None } |> RateMultDistr


    let defaultEeDistribution = EeDistribution.createBiDelta (Some 0.95)
    let defaultEeDistributionGetter = DeltaEeDistributionGetter
    let deltaRateMultDistrGetter = DeltaRateMultDistrGetter


    type ReactionRateProviderParams
        with

        static member defaultFoodCreationParam forward =
            {
                foodCreationRate = forward
            }
            |> FoodCreationRateParam

        static member defaultWasteRemovalParam forward =
            {
                wasteRemovalRate = forward
            }
            |> WasteRemovalRateParam

        static member defaultWasteRecyclingParam forward =
            {
                wasteRecyclingRate = forward
            }
            |> WasteRecyclingRateParam

        // =======================================================================================

        static member defaultSynthRndParamImpl (forward, backward) =
            {
                synthesisDistribution = Distribution.createDelta { threshold = None; scale = None; shift = Some 1.0 }
                forwardScale = Some forward
                backwardScale = Some backward
            }
            |> SynthRndParam

        static member defaultSynthRndParamImpl (forward, backward) =
            {
                synthesisDistribution = Distribution.createDelta { threshold = None; scale = None; shift = Some 1.0 }
                forwardScale = forward
                backwardScale = backward
            }
            |> SynthRndParam

        static member defaultSynthRndParam (forward : double, backward : double) =
            ReactionRateProviderParams.defaultSynthRndParamImpl (forward, backward)
            |> SynthesisRateParam

        static member defaultSynthRndParam (forward : double option, backward : double option) =
            ReactionRateProviderParams.defaultSynthRndParamImpl (forward, backward)
            |> SynthesisRateParam

        // =======================================================================================

        static member defaultSugarSynthRndParamImpl ((forward, backward), threshold) =
            {
                sugarSynthesisDistribution = Distribution.createDelta { threshold = threshold; scale = None; shift = Some 1.0 }
                forwardScale = forward
                backwardScale = backward
            }
            |> SugarSynthRndParam

        // =======================================================================================

        static member defaultDestrRndParamImpl (forward, backward) =
            {
                destructionDistribution = Distribution.createDelta { threshold = None; scale = None; shift = Some 1.0 }
                forwardScale = Some forward
                backwardScale = Some backward
            }
            |> DestrRndParam

        static member defaultDestrRndParamImpl (forward, backward) =
            {
                destructionDistribution = Distribution.createDelta { threshold = None; scale = None; shift = Some 1.0 }
                forwardScale = forward
                backwardScale = backward
            }
            |> DestrRndParam

        static member defaultDestrRndParam (forward : double, backward : double) =
            ReactionRateProviderParams.defaultDestrRndParamImpl (forward, backward)
            |> DestructionRateParam

        static member defaultDestrRndParam (forward : double option, backward : double option) =
            ReactionRateProviderParams.defaultDestrRndParamImpl (forward, backward)
            |> DestructionRateParam

        // =======================================================================================

        static member defaultCatSynthRndParamImpl (m, threshold, mult) catRateGenType =
            {
                synthesisParam = m

                catSynthRndEeParams =
                    {
                        rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                        eeDistribution = defaultEeDistribution |> Some
                    }
            }

        static member defaultCatSynthRndParam (m, threshold, mult) catRateGenType =
            ReactionRateProviderParams.defaultCatSynthRndParamImpl (m, threshold, mult) catRateGenType
            |> CatSynthRndParam
            |> CatalyticSynthesisRateParam

        static member defaultCatSynthSimParamImpl (m, threshold, mult) simThreshold catRateGenType =
            {
                catSynthParam = ReactionRateProviderParams.defaultCatSynthRndParamImpl (m, threshold, mult) (catRateGenType : CatalyticRateGenerationType)

                catSynthSimParam =
                    {
                        catRatesSimGeneration =
                            Distribution.createUniform { threshold = simThreshold; scale = None; shift = Some 1.0 }
                            |>
                            match catRateGenType.catRatesSimGenType with
                            | DistrBased -> DistributionBased
                            | FixedVal -> FixedValue

                        getEeDistr = defaultEeDistributionGetter
                        getRateMultiplierDistr = deltaRateMultDistrGetter
                    }
            }

        static member defaultCatSynthSimParam (m, threshold, mult) simThreshold catRateGenType =
            ReactionRateProviderParams.defaultCatSynthSimParamImpl (m, threshold, mult) simThreshold catRateGenType
            |> CatSynthSimParam
            |> CatalyticSynthesisRateParam

        // =======================================================================================

        static member defaultEnCatSynthRndParamImpl (m, threshold, mult) catRateGenType =
            {
                synthesisParam = m

                enCatSynthRndEeParams =
                    {
                        rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                        enEeDistribution = defaultEeDistribution |> Some
                    }
            }

        static member defaultEnCatSynthRndParam (m, threshold, mult) catRateGenType =
            ReactionRateProviderParams.defaultEnCatSynthRndParamImpl (m, threshold, mult) catRateGenType
            |> EnCatSynthRndParam
            |> EnCatalyticSynthesisRateParam

        static member defaultEnCatSynthSimParamImpl (m, threshold, mult) simThreshold catRateGenType =
            {
                enCatSynthParam = ReactionRateProviderParams.defaultEnCatSynthRndParamImpl (m, threshold, mult) (catRateGenType : CatalyticRateGenerationType)

                enCatSynthSimParam =
                    {
                        enCatRatesSimGeneration =
                            Distribution.createUniform { threshold = simThreshold; scale = None; shift = Some 1.0 }
                            |>
                            match catRateGenType.catRatesSimGenType with
                            | DistrBased -> DistributionBased
                            | FixedVal -> FixedValue

                        getRateMultiplierDistr = deltaRateMultDistrGetter
                        getForwardEeDistr = defaultEeDistributionGetter
                        getBackwardEeDistr = defaultEeDistributionGetter
                    }
            }

        static member defaultEnCatSynthSimParam (m, threshold, mult) simThreshold catRateGenType =
            ReactionRateProviderParams.defaultEnCatSynthSimParamImpl (m, threshold, mult) simThreshold catRateGenType
            |> EnCatSynthSimParam
            |> EnCatalyticSynthesisRateParam

        // =======================================================================================

        static member defaultCatDestrRndParamImpl (m, threshold, mult) catRateGenType =
            {
                destructionParam = m

                catDestrRndEeParams =
                    {
                        rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                        eeDistribution = defaultEeDistribution |> Some
                    }
            }

        static member defaultCatDestrRndParam (m, threshold, mult) catRateGenType =
            ReactionRateProviderParams.defaultCatDestrRndParamImpl (m, threshold, mult) catRateGenType
            |> CatDestrRndParam
            |> CatalyticDestructionRateParam

        static member defaultCatDestrSimParamImpl (m, threshold, mult) simThreshold catRateGenType =
            {
                catDestrParam = ReactionRateProviderParams.defaultCatDestrRndParamImpl (m, threshold, mult) (catRateGenType : CatalyticRateGenerationType)

                catDestrSimParam =
                    {
                        catRatesSimGeneration =
                            Distribution.createUniform { threshold = simThreshold; scale = None; shift = Some 1.0 }
                            |>
                            match catRateGenType.catRatesSimGenType with
                            | DistrBased -> DistributionBased
                            | FixedVal -> FixedValue

                        getEeDistr = defaultEeDistributionGetter
                        getRateMultiplierDistr = deltaRateMultDistrGetter
                    }
            }

        static member defaultCatDestrSimParam (m, threshold, mult) simThreshold catRateGenType =
            ReactionRateProviderParams.defaultCatDestrSimParamImpl (m, threshold, mult) simThreshold catRateGenType
            |> CatDestrSimParam
            |> CatalyticDestructionRateParam


        // =======================================================================================

        static member defaultEnCatDestrRndParamImpl (m, threshold, mult) catRateGenType =
            {
                destructionParam = m

                enCatDestrRndEeParams =
                    {
                        rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                        enEeDistribution = defaultEeDistribution |> Some
                    }
            }

        static member defaultEnCatDestrRndParam (m, threshold, mult) catRateGenType =
            ReactionRateProviderParams.defaultEnCatDestrRndParamImpl (m, threshold, mult) catRateGenType
            |> EnCatDestrRndParam
            |> EnCatalyticDestructionRateParam

        static member defaultEnCatDestrSimParamImpl (m, threshold, mult) simThreshold catRateGenType =
            {
                enCatDestrParam = ReactionRateProviderParams.defaultEnCatDestrRndParamImpl (m, threshold, mult) (catRateGenType : CatalyticRateGenerationType)

                enCatDestrSimParam =
                    {
                        enCatRatesSimGeneration =
                            Distribution.createUniform { threshold = simThreshold; scale = None; shift = Some 1.0 }
                            |>
                            match catRateGenType.catRatesSimGenType with
                            | DistrBased -> DistributionBased
                            | FixedVal -> FixedValue

                        getRateMultiplierDistr = deltaRateMultDistrGetter
                        getForwardEeDistr = defaultEeDistributionGetter
                        getBackwardEeDistr = defaultEeDistributionGetter
                    }
            }

        static member defaultEnCatDestrSimParam (m, threshold, mult) simThreshold catRateGenType =
            ReactionRateProviderParams.defaultEnCatDestrSimParamImpl (m, threshold, mult) simThreshold catRateGenType
            |> EnCatDestrSimParam
            |> EnCatalyticDestructionRateParam


        // =======================================================================================

        static member defaultLigRndParamImpl (forward, backward) =
            {
                ligationDistribution = Distribution.createDelta { threshold = None; scale = None; shift = Some 1.0 }
                forwardScale = Some forward
                backwardScale = Some backward
            }
            |> LigRndParam

        static member defaultLigRndParamImpl (forward, backward) =
            {
                ligationDistribution = Distribution.createDelta { threshold = None; scale = None; shift = Some 1.0 }
                forwardScale = forward
                backwardScale = backward
            }
            |> LigRndParam

        static member defaultLigRndParam (forward : double, backward : double) =
            ReactionRateProviderParams.defaultLigRndParamImpl (forward, backward)
            |> LigationRateParam

        static member defaultLigRndParam (forward : double option, backward : double option) =
            ReactionRateProviderParams.defaultLigRndParamImpl (forward, backward)
            |> LigationRateParam

        // =======================================================================================

        static member defaultCatLigRndParamImpl (m, threshold, mult) catRateGenType =
            {
                ligationParam = m

                catLigRndEeParams =
                    {
                        rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                        eeDistribution = defaultEeDistribution |> Some
                    }
            }

        static member defaultCatLigRndParam (m, threshold, mult) catRateGenType =
            ReactionRateProviderParams.defaultCatLigRndParamImpl (m, threshold, mult) catRateGenType
            |> CatLigRndParam
            |> CatalyticLigationRateParam

        static member defaultCatLigSimParamImpl (m, threshold, mult) simThreshold (catRateGenType : CatalyticRateGenerationType) =
            {
                catLigParam = ReactionRateProviderParams.defaultCatLigRndParamImpl (m, threshold, mult) catRateGenType

                catLigSimParam =
                    {
                        catRatesSimGeneration =
                            Distribution.createUniform { threshold = simThreshold; scale = None; shift = Some 1.0 }
                            |>
                            match catRateGenType.catRatesSimGenType with
                            | DistrBased -> DistributionBased
                            | FixedVal -> FixedValue

                        getEeDistr = defaultEeDistributionGetter
                        getRateMultiplierDistr = deltaRateMultDistrGetter
                    }
            }

        static member defaultCatLigSimParam (m, threshold, mult) simThreshold catRateGenType =
            ReactionRateProviderParams.defaultCatLigSimParamImpl (m, threshold, mult) simThreshold catRateGenType
            |> CatLigSimParam
            |> CatalyticLigationRateParam

        // =======================================================================================

        static member defaultEnCatLigRndParamImpl (m, threshold, mult) catRateGenType =
            {
                ligationParam = m

                enCatLigRndEeParams =
                    {
                        rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                        enEeDistribution = defaultEeDistribution |> Some
                    }
            }

        static member defaultEnCatLigRndParam (m, threshold, mult) catRateGenType =
            ReactionRateProviderParams.defaultEnCatLigRndParamImpl (m, threshold, mult) catRateGenType
            |> EnCatLigRndParam
            |> EnCatalyticLigationRateParam


        static member defaultEnCatLigSimParamImpl (m, threshold, mult) simThreshold (catRateGenType : CatalyticRateGenerationType) =
            {
                enCatLigParam = ReactionRateProviderParams.defaultEnCatLigRndParamImpl (m, threshold, mult) catRateGenType

                enCatLigSimParam =
                    {
                        enCatRatesSimGeneration =
                            Distribution.createUniform { threshold = simThreshold; scale = None; shift = Some 1.0 }
                            |>
                            match catRateGenType.catRatesSimGenType with
                            | DistrBased -> DistributionBased
                            | FixedVal -> FixedValue

                        getRateMultiplierDistr = deltaRateMultDistrGetter
                        getForwardEeDistr = defaultEeDistributionGetter
                        getBackwardEeDistr = defaultEeDistributionGetter
                    }
            }

        static member defaultEnCatLigSimParam (m, threshold, mult) simThreshold catRateGenType =
            ReactionRateProviderParams.defaultEnCatLigSimParamImpl (m, threshold, mult) simThreshold catRateGenType
            |> EnCatLigSimParam
            |> EnCatalyticLigationRateParam

        // =======================================================================================

        static member defaultSedDirRndParamImpl (threshold, mult) =
            {
                sedDirRatesEeParam =
                    {
                        sedDirRateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                        eeDistribution = defaultEeDistribution |> Some
                    }
                sedDirDistribution = Distribution.createTriangular { threshold = threshold; scale = None; shift = None }
                forwardScale = Some mult
            }

        static member defaultSedDirRndParam (threshold, mult) =
            ReactionRateProviderParams.defaultSedDirRndParamImpl (threshold, mult)
            |> SedDirRndParam
            |> SedimentationDirectRateParam

        static member defaultSedDirSimParamImpl (threshold, mult) simThreshold =
            {
                sedDirParam = ReactionRateProviderParams.defaultSedDirRndParamImpl (threshold, mult)
                sedDirSimParam =
                    {
                        sedDirSimBaseDistribution = Distribution.createUniform { threshold = simThreshold; scale = None; shift = Some 1.0 }
                        getRateMultiplierDistr = deltaRateMultDistrGetter
                        getEeDistr = defaultEeDistributionGetter
                    }
            }

        static member defaultSedDirSimParam (threshold, mult) simThreshold =
            ReactionRateProviderParams.defaultSedDirSimParamImpl (threshold, mult) simThreshold
            |> SedDirSimParam
            |> SedimentationDirectRateParam

        // =======================================================================================

        static member defaultSedAllRndParamImpl mult =
            {
                sedimentationAllDistribution = Distribution.createTriangular { threshold = None; scale = None; shift = None }
                forwardScale = Some mult
            }

        static member defaultSedAllRndParam mult =
            ReactionRateProviderParams.defaultSedAllRndParamImpl mult
            |> SedAllRndParam
            |> SedimentationAllRateParam

        // =======================================================================================

        static member defaultRacemRndParamImpl forward =
            {
                racemizationDistribution = Distribution.createDelta { threshold = None; scale = None; shift = Some 1.0 }
                forwardScale = Some forward
            }
            |> RacemRndParam

        static member defaultRacemRndParam forward =
            ReactionRateProviderParams.defaultRacemRndParamImpl forward
            |> RacemizationRateParam

        static member defaultCatRacemRndParamImpl (m, threshold, mult) catRateGenType =
            {
                racemizationParam = m
                catRacemRndEeParams =
                    {
                        rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                        eeDistribution = defaultEeDistribution |> Some
                    }
            }

        static member defaultCatRacemRndParam (m, threshold, mult) catRateGenType =
            ReactionRateProviderParams.defaultCatRacemRndParamImpl (m, threshold, mult) catRateGenType
            |> CatRacemRndParam
            |> CatalyticRacemizationRateParam

        static member defaultCatRacemSimParam (m, threshold, mult) simThreshold (catRateGenType : CatalyticRateGenerationType) =
            {
                catRacemParam = ReactionRateProviderParams.defaultCatRacemRndParamImpl (m, threshold, mult) catRateGenType

                catRacemSimParam =
                    {
                        catRatesSimGeneration =
                            Distribution.createUniform { threshold = simThreshold; scale = None; shift = Some 1.0 }
                            |>
                            match catRateGenType.catRatesSimGenType with
                            | DistrBased -> DistributionBased
                            | FixedVal -> FixedValue

                        getEeDistr = defaultEeDistributionGetter
                        getRateMultiplierDistr = deltaRateMultDistrGetter
                    }
            }
            |> CatRacemSimParam
            |> CatalyticRacemizationRateParam

        // =======================================================================================

        static member defaultEnCatRacemRndParamImpl (m, threshold, mult) catRateGenType =
            {
                racemizationParam = m

                enCatRacemRndEeParams =
                    {
                        rateMultiplierDistr = defaultRateMultiplierDistr threshold mult
                        enEeDistribution = defaultEeDistribution |> Some
                    }
            }

        static member defaultEnCatRacemRndParam (m, threshold, mult) catRateGenType =
            ReactionRateProviderParams.defaultEnCatRacemRndParamImpl (m, threshold, mult) catRateGenType
            |> EnCatRacemRndParam
            |> EnCatalyticRacemizationRateParam

        static member defaultEnCatRacemSimParamImpl (m, threshold, mult) simThreshold catRateGenType =
            {
                enCatRacemParam = ReactionRateProviderParams.defaultEnCatRacemRndParamImpl (m, threshold, mult) (catRateGenType : CatalyticRateGenerationType)

                enCatRacemSimParam =
                    {
                        enCatRatesSimGeneration =
                            Distribution.createUniform { threshold = simThreshold; scale = None; shift = Some 1.0 }
                            |>
                            match catRateGenType.catRatesSimGenType with
                            | DistrBased -> DistributionBased
                            | FixedVal -> FixedValue

                        getRateMultiplierDistr = deltaRateMultDistrGetter
                        getForwardEeDistr = defaultEeDistributionGetter
                        getBackwardEeDistr = defaultEeDistributionGetter
                    }
            }

        static member defaultEnCatRacemSimParam (m, threshold, mult) simThreshold catRateGenType =
            ReactionRateProviderParams.defaultEnCatRacemSimParamImpl (m, threshold, mult) simThreshold catRateGenType
            |> EnCatRacemSimParam
            |> EnCatalyticRacemizationRateParam
