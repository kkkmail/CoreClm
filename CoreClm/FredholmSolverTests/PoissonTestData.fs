﻿namespace FredholmSolverTests

open FredholmSolver.EeInfIntModel
open FredholmSolver.Kernel

module PoissonTestData =
    let createModelParams mp noOfDomainPoints k0 modifier : EeInfIntModelParams =
        let mp1 =
            mp
            |> EeInfIntModelParams.withDomainIntervals (DomainIntervals noOfDomainPoints)
            |> EeInfIntModelParams.withK0 k0
            |> modifier

        mp1

    /// Sets kaFuncValue to KaFuncValue.defaultQuadraticWithLinearInfValueI1 but keeps K0
    let toI1 (mp : EeInfIntModelParams) =
        let domain2D = Domain2D.create mp.eeInfModelParams.kernelParams.domainIntervals.value mp.eeInfModelParams.kernelParams.infMaxValue.value
        let k0 = mp.eeInfModelParams.kernelParams.kaFuncValue.k0

        mp
        |> EeInfIntModelParams.withKaFunc (KaFuncValue.defaultQuadraticWithLinearInfValueI1 domain2D)
        |> EeInfIntModelParams.withK0 k0

    /// Sets kaFuncValue to KaFuncValue.defaultQuadraticWithLinearInfValueI10 but keeps K0
    let toI10 (mp : EeInfIntModelParams) =
        let domain2D = Domain2D.create mp.eeInfModelParams.kernelParams.domainIntervals.value mp.eeInfModelParams.kernelParams.infMaxValue.value
        let k0 = mp.eeInfModelParams.kernelParams.kaFuncValue.k0

        mp
        |> EeInfIntModelParams.withKaFunc (KaFuncValue.defaultQuadraticWithLinearInfValueI10 domain2D)
        |> EeInfIntModelParams.withK0 k0

    // ===================================================================================

    // Flat
    // let mp_d100k10e01a0 = createModelParams EeInfIntModelParams.defaultValue 100 K0.defaultIdentityValue id
    let mp_d100k1e01a0 = createModelParams EeInfIntModelParams.defaultValue 100 K0.defaultValue id
    let mp_d100k01e01a0 = createModelParams EeInfIntModelParams.defaultValue 100 K0.defaultSmallValue id

    // ===================================================================================

    // Quadratic in inf space
    let mp_d100k1e01g01 = createModelParams EeInfIntModelParams.defaultNonLinearValue 100 K0.defaultValue id

    /// This is the main one.
    let mp_d200k1e01g01 = createModelParams EeInfIntModelParams.defaultNonLinearValue 200 K0.defaultValue id

    let mp_d200k1e01g01a001 = mp_d200k1e01g01 |> EeInfIntModelParams.withGlobalAsymmetryFactor GlobalAsymmetryFactor.defaultSmallValue
    let mp_d200k1e01g01a0001 = mp_d200k1e01g01 |> EeInfIntModelParams.withGlobalAsymmetryFactor GlobalAsymmetryFactor.defaultVerySmallValue

    let mp_d200k10e01g01 = createModelParams EeInfIntModelParams.defaultNonLinearValue 200 K0.defaultIdentityValue id

    // ===================================================================================

    // Quadratic with small linear factor in inf space.
    let mp_d100k10e01g01i01 = createModelParams EeInfIntModelParams.defaultQuadraticWithLinearInfValue 100 K0.defaultIdentityValue id
    let mp_d200k1e01g01i01 = createModelParams EeInfIntModelParams.defaultQuadraticWithLinearInfValue 200 K0.defaultValue id
    let mp_d200k10e01g01i01 = createModelParams EeInfIntModelParams.defaultQuadraticWithLinearInfValue 200 K0.defaultIdentityValue id

    // ===================================================================================

    // Quadratic with larger linear factor in inf space.
    let mp_d100k10e01g01i1 = toI1 mp_d100k10e01g01i01
    let mp_d200k1e01g01i1 = toI1 mp_d200k1e01g01i01
    let mp_d200k10e01g01i1 = toI1 mp_d200k10e01g01i01

    // ===================================================================================

    // Quadratic with even larger linear factor in inf space.
    let mp_d100k10e01g01i10 = toI10 mp_d100k10e01g01i01
    let mp_d200k1e01g01i10 = toI10 mp_d200k1e01g01i01
    let mp_d200k10e01g01i10 = toI10 mp_d200k10e01g01i01

    // ===================================================================================

    // 1T
    let mp_d200k1e01g01f1T = mp_d200k1e01g01.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d200k10e01g01i1f1T = mp_d200k10e01g01i1.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d200k10e01g01i10f1T = mp_d200k10e01g01i10.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d200k1e01g01a001f1T = mp_d200k1e01g01a001.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d200k1e01g01a0001f1T = mp_d200k1e01g01a0001.withTotalMolecules MoleculeCount.OneTrillion

    // ===================================================================================

    // 1P
    let mp_d200k1e01g01f1P = mp_d200k1e01g01.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d200k10e01g01i1f1P = mp_d200k10e01g01i1.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d200k10e01g01i10f1P = mp_d200k10e01g01i10.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d200k1e01g01a001f1P = mp_d200k1e01g01a001.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d200k1e01g01a0001f1P = mp_d200k1e01g01a0001.withTotalMolecules MoleculeCount.OneQuadrillion

    // ===================================================================================

    // 1E
    let mp_d200k1e01g01f1E = mp_d200k1e01g01.withTotalMolecules MoleculeCount.OneQuintillion
    let mp_d200k10e01g01i1f1E = mp_d200k10e01g01i1.withTotalMolecules MoleculeCount.OneQuintillion
    let mp_d200k10e01g01i10f1E = mp_d200k10e01g01i10.withTotalMolecules MoleculeCount.OneQuintillion
    let mp_d200k1e01g01a001f1E = mp_d200k1e01g01a001.withTotalMolecules MoleculeCount.OneQuintillion
    let mp_d200k1e01g01a0001f1E = mp_d200k1e01g01a0001.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================