namespace FredholmSolverTests

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
    // ===================================================================================

    // e = 0.005

    /// This is the main one for e = 0.005, a = 0.0001.
    let mp_d200k1e005g01a0001 =
        createModelParams EeInfIntModelParams.defaultNonLinearValue 200 K0.defaultValue (EeInfIntModelParams.withEps0 Eps0.defaultNarrowValue)
        |> EeInfIntModelParams.withGlobalAsymmetryFactor GlobalAsymmetryFactor.defaultVerySmallValue

    let mp_d200k1e005g01a0001f1T = mp_d200k1e005g01a0001.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d200k1e005g01a0001f1P = mp_d200k1e005g01a0001.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d200k1e005g01a0001f1E = mp_d200k1e005g01a0001.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================

    /// This is the main one for e = 0.005, a = 0.0001, i = 0.1.
    let mp_d200k1e005g01a0001i1 = toI1 mp_d200k1e005g01a0001

    let mp_d200k1e005g01a0001i1f1T = mp_d200k1e005g01a0001i1.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d200k1e005g01a0001i1f1P = mp_d200k1e005g01a0001i1.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d200k1e005g01a0001i1f1E = mp_d200k1e005g01a0001i1.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================

    /// This is the main one for e = 0.005, a = 0.0001, i = 1.0.
    let mp_d200k1e005g01a0001i10 = toI10 mp_d200k1e005g01a0001

    let mp_d200k1e005g01a0001i10f1T = mp_d200k1e005g01a0001i10.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d200k1e005g01a0001i10f1P = mp_d200k1e005g01a0001i10.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d200k1e005g01a0001i10f1E = mp_d200k1e005g01a0001i10.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================
    // ===================================================================================

    // e = 0.005

    /// This is the main one for e = 0.005, a = 0.001.
    let mp_d200k1e005g01a001 =
        createModelParams EeInfIntModelParams.defaultNonLinearValue 200 K0.defaultValue (EeInfIntModelParams.withEps0 Eps0.defaultNarrowValue)
        |> EeInfIntModelParams.withGlobalAsymmetryFactor GlobalAsymmetryFactor.defaultSmallValue

    let mp_d200k1e005g01a001f1T = mp_d200k1e005g01a001.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d200k1e005g01a001f1P = mp_d200k1e005g01a001.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d200k1e005g01a001f1E = mp_d200k1e005g01a001.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================

    /// This is the main one for e = 0.005, a = 0.001, i = 0.1.
    let mp_d200k1e005g01a001i1 = toI1 mp_d200k1e005g01a001

    let mp_d200k1e005g01a001i1f1T = mp_d200k1e005g01a001i1.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d200k1e005g01a001i1f1P = mp_d200k1e005g01a001i1.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d200k1e005g01a001i1f1E = mp_d200k1e005g01a001i1.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================

    /// This is the main one for e = 0.005, a = 0.001, i = 1.0.
    let mp_d200k1e005g01a001i10 = toI10 mp_d200k1e005g01a001

    let mp_d200k1e005g01a001i10f1T = mp_d200k1e005g01a001i10.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d200k1e005g01a001i10f1P = mp_d200k1e005g01a001i10.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d200k1e005g01a001i10f1E = mp_d200k1e005g01a001i10.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================
    // ===================================================================================

    /// D = 300, e = 0.005, a = 0.0001.
    let mp_d300k1e005g01a0001 =
        createModelParams EeInfIntModelParams.defaultNonLinearValue 300 K0.defaultValue (EeInfIntModelParams.withEps0 Eps0.defaultNarrowValue)
        |> EeInfIntModelParams.withGlobalAsymmetryFactor GlobalAsymmetryFactor.defaultVerySmallValue

    let mp_d300k1e005g01a0001f1T = mp_d300k1e005g01a0001.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d300k1e005g01a0001f1P = mp_d300k1e005g01a0001.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d300k1e005g01a0001f1E = mp_d300k1e005g01a0001.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================
    // ===================================================================================

    /// D = 500, e = 0.005, a = 0.0001.
    let mp_d500k1e005g01a0001 =
        createModelParams EeInfIntModelParams.defaultNonLinearValue 500 K0.defaultValue (EeInfIntModelParams.withEps0 Eps0.defaultNarrowValue)
        |> EeInfIntModelParams.withGlobalAsymmetryFactor GlobalAsymmetryFactor.defaultVerySmallValue

    let mp_d500k1e005g01a0001f1T = mp_d500k1e005g01a0001.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d500k1e005g01a0001f1P = mp_d500k1e005g01a0001.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d500k1e005g01a0001f1E = mp_d500k1e005g01a0001.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================
    // ===================================================================================

    /// D = 500, e = 0.01, a = 0.0001.
    let mp_d500k1e01g01a0001 =
        createModelParams EeInfIntModelParams.defaultNonLinearValue 500 K0.defaultValue (EeInfIntModelParams.withEps0 Eps0.defaultValue)
        |> EeInfIntModelParams.withGlobalAsymmetryFactor GlobalAsymmetryFactor.defaultVerySmallValue

    let mp_d500k1e01g01a0001f1T = mp_d500k1e01g01a0001.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d500k1e01g01a0001f1P = mp_d500k1e01g01a0001.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d500k1e01g01a0001f1E = mp_d500k1e01g01a0001.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================
    // ===================================================================================

    /// D = 500, e = 0.005, a = 0.001.
    let mp_d500k1e005g01a001 =
        createModelParams EeInfIntModelParams.defaultNonLinearValue 500 K0.defaultValue (EeInfIntModelParams.withEps0 Eps0.defaultNarrowValue)
        |> EeInfIntModelParams.withGlobalAsymmetryFactor GlobalAsymmetryFactor.defaultSmallValue

    let mp_d500k1e005g01a001f1T = mp_d500k1e005g01a001.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d500k1e005g01a001f1P = mp_d500k1e005g01a001.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d500k1e005g01a001f1E = mp_d500k1e005g01a001.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================
    // ===================================================================================

    /// D = 500, e = 0.005, a = 0.002.
    let mp_d500k1e005g01a002 =
        createModelParams EeInfIntModelParams.defaultNonLinearValue 500 K0.defaultValue (EeInfIntModelParams.withEps0 Eps0.defaultNarrowValue)
        |> EeInfIntModelParams.withGlobalAsymmetryFactor GlobalAsymmetryFactor.defaultSmallValueX2

    let mp_d500k1e005g01a002f1T = mp_d500k1e005g01a002.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d500k1e005g01a002f1P = mp_d500k1e005g01a002.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d500k1e005g01a002f1E = mp_d500k1e005g01a002.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================
    // ===================================================================================

    /// D = 500, e = 0.005, a = 0.005.
    let mp_d500k1e005g01a005 =
        createModelParams EeInfIntModelParams.defaultNonLinearValue 500 K0.defaultValue (EeInfIntModelParams.withEps0 Eps0.defaultNarrowValue)
        |> EeInfIntModelParams.withGlobalAsymmetryFactor GlobalAsymmetryFactor.defaultSmallValueX5

    let mp_d500k1e005g01a005f1T = mp_d500k1e005g01a005.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d500k1e005g01a005f1P = mp_d500k1e005g01a005.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d500k1e005g01a005f1E = mp_d500k1e005g01a005.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================
    // ===================================================================================

    /// D = 500, e = 0.01, a = 0.001.
    let mp_d500k1e01g01a001 =
        createModelParams EeInfIntModelParams.defaultNonLinearValue 500 K0.defaultValue (EeInfIntModelParams.withEps0 Eps0.defaultValue)
        |> EeInfIntModelParams.withGlobalAsymmetryFactor GlobalAsymmetryFactor.defaultSmallValue

    let mp_d500k1e01g01a001f1T = mp_d500k1e01g01a001.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d500k1e01g01a001f1P = mp_d500k1e01g01a001.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d500k1e01g01a001f1E = mp_d500k1e01g01a001.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================
    // ===================================================================================

    /// D = 500, e = 0.01, a = 0.002.
    let mp_d500k1e01g01a002 =
        createModelParams EeInfIntModelParams.defaultNonLinearValue 500 K0.defaultValue (EeInfIntModelParams.withEps0 Eps0.defaultValue)
        |> EeInfIntModelParams.withGlobalAsymmetryFactor GlobalAsymmetryFactor.defaultSmallValueX2

    let mp_d500k1e01g01a002f1T = mp_d500k1e01g01a002.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d500k1e01g01a002f1P = mp_d500k1e01g01a002.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d500k1e01g01a002f1E = mp_d500k1e01g01a002.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================
    // ===================================================================================

    /// D = 500, e = 0.01, a = 0.005.
    let mp_d500k1e01g01a005 =
        createModelParams EeInfIntModelParams.defaultNonLinearValue 500 K0.defaultValue (EeInfIntModelParams.withEps0 Eps0.defaultValue)
        |> EeInfIntModelParams.withGlobalAsymmetryFactor GlobalAsymmetryFactor.defaultSmallValueX5

    let mp_d500k1e01g01a005f1T = mp_d500k1e01g01a005.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d500k1e01g01a005f1P = mp_d500k1e01g01a005.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d500k1e01g01a005f1E = mp_d500k1e01g01a005.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================
    // ===================================================================================

    /// D = 500, e = 0.005, a = 0.0001, i = 1.0.
    let mp_d500k1e005g01a0001i10 = toI10 mp_d500k1e005g01a0001

    let mp_d500k1e005g01a0001i10f1T = mp_d500k1e005g01a0001i10.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d500k1e005g01a0001i10f1P = mp_d500k1e005g01a0001i10.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d500k1e005g01a0001i10f1E = mp_d500k1e005g01a0001i10.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================
    // ===================================================================================

    /// D = 500, e = 0.01, a = 0.0001, i = 1.0.
    let mp_d500k1e01g01a0001i10 = toI10 mp_d500k1e01g01a0001

    let mp_d500k1e01g01a0001i10f1T = mp_d500k1e01g01a0001i10.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d500k1e01g01a0001i10f1P = mp_d500k1e01g01a0001i10.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d500k1e01g01a0001i10f1E = mp_d500k1e01g01a0001i10.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================
    // ===================================================================================

    /// D = 500, e = 0.005, a = 0.001, i = 1.0.
    let mp_d500k1e005g01a001i10 = toI10 mp_d500k1e005g01a001

    let mp_d500k1e005g01a001i10f1T = mp_d500k1e005g01a001i10.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d500k1e005g01a001i10f1P = mp_d500k1e005g01a001i10.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d500k1e005g01a001i10f1E = mp_d500k1e005g01a001i10.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================
    // ===================================================================================

    /// D = 500, e = 0.005, a = 0.002, i = 1.0.
    let mp_d500k1e005g01a002i10 = toI10 mp_d500k1e005g01a002

    let mp_d500k1e005g01a002i10f1T = mp_d500k1e005g01a002i10.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d500k1e005g01a002i10f1P = mp_d500k1e005g01a002i10.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d500k1e005g01a002i10f1E = mp_d500k1e005g01a002i10.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================
    // ===================================================================================

    /// D = 500, e = 0.005, a = 0.005, i = 1.0.
    let mp_d500k1e005g01a005i10 = toI10 mp_d500k1e005g01a005

    let mp_d500k1e005g01a005i10f1T = mp_d500k1e005g01a005i10.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d500k1e005g01a005i10f1P = mp_d500k1e005g01a005i10.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d500k1e005g01a005i10f1E = mp_d500k1e005g01a005i10.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================
    // ===================================================================================

    /// D = 500, e = 0.01, a = 0.001, i = 1.0.
    let mp_d500k1e01g01a001i10 = toI10 mp_d500k1e01g01a001

    let mp_d500k1e01g01a001i10f1T = mp_d500k1e01g01a001i10.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d500k1e01g01a001i10f1P = mp_d500k1e01g01a001i10.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d500k1e01g01a001i10f1E = mp_d500k1e01g01a001i10.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================
    // ===================================================================================

    /// D = 500, e = 0.01, a = 0.002, i = 1.0.
    let mp_d500k1e01g01a002i10 = toI10 mp_d500k1e01g01a002

    let mp_d500k1e01g01a002i10f1T = mp_d500k1e01g01a002i10.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d500k1e01g01a002i10f1P = mp_d500k1e01g01a002i10.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d500k1e01g01a002i10f1E = mp_d500k1e01g01a002i10.withTotalMolecules MoleculeCount.OneQuintillion

    // ===================================================================================
    // ===================================================================================

    /// D = 500, e = 0.01, a = 0.005, i = 1.0.
    let mp_d500k1e01g01a005i10 = toI10 mp_d500k1e01g01a005

    let mp_d500k1e01g01a005i10f1T = mp_d500k1e01g01a005i10.withTotalMolecules MoleculeCount.OneTrillion
    let mp_d500k1e01g01a005i10f1P = mp_d500k1e01g01a005i10.withTotalMolecules MoleculeCount.OneQuadrillion
    let mp_d500k1e01g01a005i10f1E = mp_d500k1e01g01a005i10.withTotalMolecules MoleculeCount.OneQuintillion
