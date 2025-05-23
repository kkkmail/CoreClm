﻿namespace FredholmSolver

open System
open Softellect.Sys.Primitives
open Softellect.Sys.Core
open Softellect.Math.Primitives
open Softellect.Math.Models
open FredholmSolver.Primitives
open FredholmSolver.Sparse

module Kernel =

    let radius (a : double) (b : double) = sqrt (a * a + b * b)


    /// Max value of the range in inf space.
    type InfMaxValue =
        | InfMaxValue of double

        member r.value = let (InfMaxValue v) = r in v
        static member defaultValue = InfMaxValue 25.0


    /// Describes a function domain suitable for integral approximation.
    /// Equidistant grid is used to reduce the number of multiplications.
    type Domain
        with
        member private d.integralValue xSize (v : SparseValue<double>) =
            match v.i = 0, v.i = xSize with
            | true, false -> 0.5 * v.value1D
            | false, true -> 0.5 * v.value1D
            | _ -> v.value1D

        // member d.noOfIntervals : int = d.points.Length - 1

        member d.integrateValues (v : SparseArray<double>) =
            let len = d.noOfIntervals.value
            let sum = v.value |> Array.map (fun e -> d.integralValue len e) |> Array.sum
            sum * d.step

        /// Number of points is (noOfIntervals + 1).
        static member create (noOfIntervals, minValue, maxValue) =
            Domain.create(DomainIntervals noOfIntervals, { minValue = minValue; maxValue = maxValue })


    /// Data that describes a rectangle in ee * inf space.
    /// ee space is naturally limited to [-1, 1] unless we use a conformal-like transformation to extend it to [-Infinity, Infinity].
    /// inf (information) space is naturally limited at the lower bound (0). The upper bound can be rescaled to any number or even taken to Infinity.
    type Domain2D
        with
        member d.eeDomain = d.d0
        member d.infDomain = d.d1

        member private d.normalize v = v * d.eeDomain.step * d.infDomain.step

        member private _.integralValue xSize ySize (v : SparseValue2D<double>) =
            match v.i = 0, v.j = 0, v.i = xSize, v.j = ySize with
            | true, true, false, false -> 0.25 * v.value2D
            | true, false, false, true -> 0.25 * v.value2D
            | false, true, true, false -> 0.25 * v.value2D
            | false, false, true, true -> 0.25 * v.value2D

            | true, false, false, false -> 0.5 * v.value2D
            | false, true, false, false -> 0.5 * v.value2D
            | false, false, true, false -> 0.5 * v.value2D
            | false, false, false, true -> 0.5 * v.value2D

            | _ -> v.value2D

        member private d.integrateValues (a : double[][])=
            let len1 = a.Length - 1
            let len2 = a[0].Length - 1
            let sum = a |> Array.map (fun e -> e |> Array.sum) |> Array.sum
            let edgeSum1 = a[0] |> Array.sum
            let edgeSum2 = a[len1] |> Array.sum
            let edgeSum3 = a |> Array.mapi (fun i _ -> a[i][0]) |> Array.sum
            let edgeSum4 = a |> Array.mapi (fun i _ -> a[i][len2]) |> Array.sum
            let cornerSum = a[0][0] + a[len1][0] + a[0][len2] + a[len1][len2]
            let retVal = (4.0 * sum - 2.0 * (edgeSum1 + edgeSum2 + edgeSum3 + edgeSum4) + cornerSum) / 4.0 |> d.normalize
            retVal

        member d.integrateValues (a : SparseArray2D<double>) =
            let len1 = d.eeDomain.points.value.Length - 1
            let len2 = d.infDomain.points.value.Length - 1
            let integralValue = d.integralValue len1 len2
            let sum = a.value |> Array.map integralValue |> Array.sum
            let retVal = sum |> d.normalize
            retVal

        member private d.integrateValues (a : SparseArray2D<double>, b : LinearMatrix<double>) =
            let bValue = b.getValue
            let len1 = d.eeDomain.points.value.Length - 1
            let len2 = d.infDomain.points.value.Length - 1
            let integralValue = d.integralValue len1 len2
            let sum = a.value |> Array.map (fun e -> (integralValue e) * (bValue e.i e.j) ) |> Array.sum
            let retVal = sum |> d.normalize
            retVal

        /// Calculates how many protocells are created for a given "point".
        member private d.evolve(p : PoissonSingleSampler, multiplier : double, a : SparseArray2D<double>, b : Matrix<int64>) =
            let g (e : SparseValue2D<double>) =
                let n = b.value[e.i][e.j]

                if n <= 0L then 0L
                else
                    let lambda = (double n) * e.value2D * multiplier
                    let retVal = if lambda > 0.0 then p.nextPoisson lambda else 0L
                    retVal

            let sum = a.value |> Array.map g |> Array.sum
            sum

        /// Calculates how many protocells are created for a given "point".
        member private d.evolve(p : PoissonSingleSampler, a : SparseArray2D<double>, b : LinearMatrix<int64>) =
            let bValue = b.getValue

            let g (e : SparseValue2D<double>) =
                let n = bValue e.i e.j

                if n <= 0L then 0L
                else
                    let lambda = (double n) * e.value2D
                    let retVal = if lambda > 0.0 then p.nextPoisson lambda else 0L
                    retVal

            let sum = a.value |> Array.map g |> Array.sum
            sum

        member d.integrateValues (a : Matrix<double>) = d.integrateValues a.value

        member d.integrateValues (a : LinearMatrix<double>) =
            let len1 = a.d1 - 1
            let len2 = a.d2 - 1
            let r1 = a.d1Range
            let r2 = a.d2Range

            let sum = r1 |> Array.map (fun i -> r2 |> Array.map (fun j -> a.getValue i j) |> Array.sum) |> Array.sum
            let edgeSum1 = r2 |> Array.map (fun j -> a.getValue 0 j) |> Array.sum
            let edgeSum2 = r2 |> Array.map (fun j -> a.getValue len1 j) |> Array.sum
            let edgeSum3 = r1 |> Array.map (fun i -> a.getValue i 0) |> Array.sum
            let edgeSum4 = r1 |> Array.map (fun i -> a.getValue i len2) |> Array.sum
            let cornerSum = (a.getValue 0 0) + (a.getValue len1 0) + (a.getValue 0 len2) + (a.getValue len1 len2)
            let retVal = (4.0 * sum - 2.0 * (edgeSum1 + edgeSum2 + edgeSum3 + edgeSum4) + cornerSum) / 4.0 |> d.normalize
            retVal

        // member private d.integrateValues (a : double[][]) = a |> Array.map (fun e -> e |> Array.sum) |> Array.sum |> d.normalize
        // member private d.integrateValues (a : SparseArray2D<double>) = a.value |> Array.map (fun e -> e.value2D) |> Array.sum |> d.normalize
        //
        // member private d.integrateValues (a : SparseArray2D<double>, b : LinearMatrix<double>) =
        //     let bValue = b.getValue
        //     let sum = a.value |> Array.map (fun e -> e.value2D * (bValue e.i e.j)) |> Array.sum |> d.normalize
        //     sum
        //
        // member d.integrateValues (a : Matrix<double>) =
        //     let sum = a.value |> Array.map (fun e -> e |> Array.sum) |> Array.sum |> d.normalize
        //     sum
        //
        // member d.integrateValues (a : LinearMatrix<double>) =
        //     let sum = a.d1Range |> Array.map (fun i -> a.d2Range |> Array.map (fun j -> a.getValue i j) |> Array.sum) |> Array.sum |> d.normalize
        //     sum

        member d.integrateValues (a : SparseArray4D<double>, b : LinearMatrix<double>) =
            a.value |> Array.map (fun v -> v |> Array.map (fun e -> d.integrateValues (e, b))) |> Matrix

        // /// Calculates how many protocells are created.
        // member d.evolve (useParallel: bool, p : PoissonSampler, multiplier : double, a : SparseArray4D<double>, b : Matrix<int64>) =
        //     let evolveFunc i v = v |> Array.map (fun e -> d.evolve (p.getSampler i, multiplier, e, b))
        //
        //     if useParallel then
        //         let result = Array.zeroCreate a.value.Length
        //         let parallelOptions = ParallelOptions()
        //         parallelOptions.MaxDegreeOfParallelism <- 18 // Set the number of cores to use
        //         Parallel.For(0, a.value.Length, parallelOptions, fun i -> result.[i] <- evolveFunc i a.value[i]) |> ignore
        //         result |> Matrix
        //     else
        //         a.value |> Array.mapi evolveFunc |> Matrix

        member d.evolve (useParallel: bool, p : PoissonSampler, multiplier : double, a : SparseArray4D<double>, b : Matrix<int64>) =
            let mapi = if useParallel then Array.Parallel.mapi else Array.mapi
            a.value |> mapi (fun i v -> v |> Array.map (fun e -> d.evolve (p.getSampler i, multiplier, e, b))) |> Matrix

        // /// Calculates how many protocells are created.
        // member d.evolve (p : PoissonSampler, a : SparseArray4D<double>, b : LinearMatrix<int64>) =
        //     a.value |> Array.map (fun v -> v |> Array.map (fun e -> d.evolve (p, e, b))) |> Matrix

        member d.integrateValues (a : SparseArray4D<double>) =
            a.value |> Array.map (fun v -> v |> Array.map d.integrateValues) |> Matrix

        member d.norm (a : LinearMatrix<double>) = d.integrateValues a

        member d.mean (a : LinearMatrix<double>) =
            let norm = d.norm a

            if norm > 0.0
            then
                let mx = (d.integrateValues (d.eeDomain.points * a)) / norm
                let my = (d.integrateValues (a * d.infDomain.points)) / norm
                (mx, my)
            else (0.0, 0.0)

        member d.mean (a : Matrix<int64>) =
            let norm = a.total()
            let b = a.convert double

            if norm > 0L
            then
                let mx = (d.eeDomain.points * b).total() / (double norm)
                let my = (b * d.infDomain.points).total() / (double norm)
                (mx, my)
            else (0.0, 0.0)

        member d.stdDev (a : LinearMatrix<double>) =
            let norm = d.norm a

            if norm > 0.0
            then
                let mx, my = d.mean a
                let m2x = (d.integrateValues (d.eeDomain.points * (d.eeDomain.points * a))) / norm
                let m2y = (d.integrateValues ((a * d.infDomain.points) * d.infDomain.points)) / norm
                (Math.Max(m2x - mx * mx, 0.0) |> Math.Sqrt, Math.Max(m2y - my * my, 0.0) |> Math.Sqrt)
            else (0.0, 0.0)

        member d.stdDev (a : Matrix<int64>) =
            let norm = a.total()
            let b = a.convert double

            if norm > 0L
            then
                let mx, my = d.mean a
                let m2x = (d.eeDomain.points * (d.eeDomain.points * b)).total() / (double norm)
                let m2y = ((b * d.infDomain.points) * d.infDomain.points).total() / (double norm)
                (Math.Max(m2x - mx * mx, 0.0) |> Math.Sqrt, Math.Max(m2y - my * my, 0.0) |> Math.Sqrt)
            else (0.0, 0.0)

        // static member eeMinValue = -1.0
        // static member eeMaxValue = 1.0
        // static member infDefaultMinValue = 0.0
        // static member defaultRanges = [| Domain2D.eeMinValue; Domain2D.eeMaxValue; Domain2D.infDefaultMinValue; InfMaxValue.defaultValue.value |]

        static member defaultDomainRange =
            {
                r0 = DomainRange.defaultValue
                r1 = { minValue = 0.0; maxValue = InfMaxValue.defaultValue.value }
            }

        static member defaultRanges =
            [|
                Domain2D.defaultDomainRange.r0.minValue
                Domain2D.defaultDomainRange.r0.maxValue
                Domain2D.defaultDomainRange.r1.minValue
                Domain2D.defaultDomainRange.r1.maxValue
            |]
        member d.ranges = [| d.eeDomain.points.value[0]; Array.last d.eeDomain.points.value; d.infDomain.points.value[0]; Array.last d.infDomain.points.value |]

        // static member create (noOfIntervals, l2) =
        //     let eeDomain = Domain.create (noOfIntervals, Domain2D.eeMinValue, Domain2D.eeMaxValue)
        //     let infDomain = Domain.create (noOfIntervals, Domain2D.infDefaultMinValue, l2)
        //
        //     {
        //         d0 = eeDomain
        //         d1 = infDomain
        //     }

        static member defaultValue = Domain2D.create (DomainIntervals.defaultValue, Domain2D.defaultDomainRange)

        member d.modelString =
            let a =
                if d.eeDomain.noOfIntervals = d.infDomain.noOfIntervals
                then $"d{d.eeDomain.noOfIntervals}"
                else $"d{d.eeDomain.noOfIntervals}x{d.infDomain.noOfIntervals}"

            match toModelStringArray Domain2D.defaultRanges d.ranges with
            | Some b -> $"{a}_{b}"
            | None -> a


    let factorial n = [ 1..n ] |> List.fold (*) 1


    /// Uses: x1 = xScale * (x - x0) in Taylor expansion.
    type TaylorApproximation =
        {
            x0 : double
            xScale : double
            coefficients : double[]
        }

        member ta.calculate x =
            let x1 = ta.xScale * (x - ta.x0)

            let retVal =
                ta.coefficients
                |> Array.mapi (fun i e -> (pown x1 i) * e / (factorial i |> double))
                |> Array.sum

            retVal

        member ta.comparisonFactors = [| ta.x0; ta.xScale |]


    type ScaledTaylorApproximation =
        {
            scale : double
            taylorApproximation : TaylorApproximation
        }

        member ta.comparisonFactors = Array.concat [| [| ta.scale |];  ta.taylorApproximation.comparisonFactors |]


    /// Uses: x1 = xScale * (x - x0) and y1 = yScale * (y - y0) in Taylor expansion.
    ///
    /// Each sub-array should contain the coefficients for all terms of a particular total order.
    /// For example, if the highest order is 2, coefficients should be initialized as
    /// [| [|a00|]; [|a10; a01|]; [|a20; a11; a02|] |],
    /// where a20 is the coefficient of x^2, a11 of x * y, etc.
    /// Note that the binomial coefficient is not included in the coefficients.
    type TaylorApproximation2D =
        {
            x0 : double
            y0 : double
            xScale : double
            yScale : double
            coefficients : double[][]
        }

        member ta.calculate (x, y) =
            let x1 = ta.xScale * (x - ta.x0)
            let y1 = ta.yScale * (y - ta.y0)

            let retVal =
                ta.coefficients
                |> Array.mapi (fun i row ->
                    row
                    |> Array.mapi (fun j e ->
                        let binomial = factorial(i) / (factorial(j) * factorial(i - j)) |> double
                        (pown x1 j) * (pown y1 (i - j)) *  binomial * e / (factorial i |> double)))
                |> Array.concat
                |> Array.sum

            retVal

        member ta.comparisonFactors = [| ta.x0; ta.xScale; ta.y0; ta.yScale |]


    /// Separate Taylor approximations for ee and inf spaces.
    type EeInfTaylorApproximation =
        {
            tEe : TaylorApproximation
            tInf : TaylorApproximation
        }


    type ScaledEeInfTaylorApproximation =
        {
            eeInfScale : double
            tEeInf : EeInfTaylorApproximation
        }

        member ta.comparisonFactors = Array.concat [| ta.tEeInf.tEe.comparisonFactors;  ta.tEeInf.tInf.comparisonFactors |]


    type ScaledEeInfTaylorApproximation2D =
        {
            eeInfScale : double
            tEeInf2D : TaylorApproximation2D
        }

        member ta.comparisonFactors = ta.tEeInf2D.comparisonFactors


    /// TODO kk:20231017 - Only scalar eps is supported for now.
    /// Type to describe a function used to calculate eps in mutation probability calculations.
    type EpsFunc =
        | EpsFunc of (Domain -> double -> double)

        member r.invoke = let (EpsFunc v) = r in v


    /// k0 multiplier in kA.
    type K0 =
        | K0 of double

        member r.value = let (K0 v) = r in v
        static member defaultIdentityValue = K0 1.0
        static member defaultValue = K0 0.1
        static member defaultSmallValue = K0 0.01
        static member defaultVerySmallValue = K0 0.001


    /// ka portion of the kernel.
    type KaFunc =
        | KaFunc of (Domain2D -> double -> double -> double)

        member r.invoke = let (KaFunc v) = r in v


    /// Decay function.
    type GammaFunc =
        | GammaFunc of (Domain2D -> double -> double -> double)

        member r.invoke = let (GammaFunc v) = r in v


    type Eps0 =
        | Eps0 of double

        member r.value = let (Eps0 v) = r in v
        static member defaultValue = Eps0 0.01
        static member defaultNarrowValue = Eps0 0.005
        static member defaultWideValue = Eps0 0.02
        member e.modelString = toModelString Eps0.defaultValue.value e.value |> bindPrefix "e"


    type EpsFuncValue =
        | ScalarEps of Eps0

        member ef.epsFunc (_ : Domain) : EpsFunc =
            match ef with
            | ScalarEps e -> EpsFunc (fun _ _ -> e.value)

        member ef.modelString =
            match ef with
            | ScalarEps e -> e.modelString |> Option.defaultValue EmptyString

    let separableFunc (v : EeInfTaylorApproximation) a b =
        let eeVal = v.tEe.calculate a
        let infVal = v.tInf.calculate b
        eeVal * infVal


    /// We want (2 / 3) of the domain range to scale to 1.0.
    let twoThirdInfScale (d : Domain2D) =
        let one = (2.0 / 3.0) * d.infDomain.domainRange.range
        let scale = 1.0 / one
        scale


    type KaFuncValue =
        | IdentityKa of double
        | SeparableKa of ScaledEeInfTaylorApproximation
        | SphericalKa of ScaledTaylorApproximation

        member k.kaFunc (_ : Domain2D) : KaFunc =
            match k with
            | IdentityKa v -> (fun _ _ _ -> v)
            | SeparableKa v -> (fun _ a b -> v.eeInfScale * (separableFunc v.tEeInf a b))
            | SphericalKa e -> fun _ a b -> e.scale * (e.taylorApproximation.calculate (radius a b))
            |> KaFunc

        /// Same as kaFunc but without k0.
        member k.kaFuncUnscaled (_ : Domain2D) : KaFunc =
            match k with
            | IdentityKa _ -> (fun _ _ _ -> 1.0)
            | SeparableKa v -> (fun _ a b -> separableFunc v.tEeInf a b)
            | SphericalKa e -> fun _ a b -> e.taylorApproximation.calculate (radius a b)
            |> KaFunc

        /// k0 - effective y scale of ka.
        member k.k0 =
            match k with
            | IdentityKa v -> v
            | SeparableKa v -> v.eeInfScale
            | SphericalKa e -> e.scale
            |> K0

        /// Changes k0 value.
        member k.withK0 (k0 : K0) =
            match k with
            | IdentityKa _ -> IdentityKa k0.value
            | SeparableKa v -> SeparableKa { v with eeInfScale = k0.value }
            | SphericalKa v -> SphericalKa { v with scale = k0.value }

        member k.comparisonFactors =
            match k with
            | IdentityKa _ -> [||]
            | SeparableKa v -> v.comparisonFactors
            | SphericalKa v -> v.comparisonFactors

        /// Default value is 1.0 on all domain.
        static member defaultValue = IdentityKa 1.0

        static member defaultQuadraticCoefficients = [| 1.0; 0.0; 1.0 |]

        /// Quadratic growth from (0, 0) point.
        static member defaultQuadraticValue (d : Domain2D) =
            let tEe =
                {
                    x0 = 0.0
                    xScale = 1.0
                    coefficients = KaFuncValue.defaultQuadraticCoefficients
                }

            let tInf =
                {
                    x0 = 0.0
                    xScale = twoThirdInfScale d
                    coefficients = KaFuncValue.defaultQuadraticCoefficients
                }

            SeparableKa { eeInfScale = K0.defaultValue.value; tEeInf = { tEe = tEe; tInf = tInf } }

        static member defaultSymmetricQuadraticValue (d : Domain2D) =
            let ta =
                {
                    x0 = 0.0
                    xScale = 1.0
                    coefficients = KaFuncValue.defaultQuadraticCoefficients
                }

            SphericalKa { scale = K0.defaultValue.value; taylorApproximation = ta }

        /// Quadratic growth from (0, 0) point with a 0.01 linear growth in inf space.
        static member defaultQuadraticWithLinearInfValue (d : Domain2D) =
            let tEe =
                {
                    x0 = 0.0
                    xScale = 1.0
                    coefficients = [| 1.0; 0.0; 1.0 |]
                }

            let tInf =
                {
                    x0 = 0.0
                    xScale = twoThirdInfScale d
                    coefficients = [| 1.0; 0.01; 1.0 |]
                }

            SeparableKa { eeInfScale = K0.defaultValue.value; tEeInf = { tEe = tEe; tInf = tInf } }

        /// Quadratic growth from (0, 0) point with a 0.1 linear growth in inf space.
        static member defaultQuadraticWithLinearInfValueI1 (d : Domain2D) =
            let tEe =
                {
                    x0 = 0.0
                    xScale = 1.0
                    coefficients = [| 1.0; 0.0; 1.0 |]
                }

            let tInf =
                {
                    x0 = 0.0
                    xScale = twoThirdInfScale d
                    coefficients = [| 1.0; 0.1; 1.0 |]
                }

            SeparableKa { eeInfScale = K0.defaultValue.value; tEeInf = { tEe = tEe; tInf = tInf } }

        /// Quadratic growth from (0, 0) point with a 1.0 linear growth in inf space.
        static member defaultQuadraticWithLinearInfValueI10 (d : Domain2D) =
            let tEe =
                {
                    x0 = 0.0
                    xScale = 1.0
                    coefficients = [| 1.0; 0.0; 1.0 |]
                }

            let tInf =
                {
                    x0 = 0.0
                    xScale = twoThirdInfScale d
                    coefficients = [| 1.0; 1.0; 1.0 |]
                }

            SeparableKa { eeInfScale = K0.defaultValue.value; tEeInf = { tEe = tEe; tInf = tInf } }

        /// defaultQuadraticValue for a default Domain2D is the comparison value here.
        member k.modelString =
            match k with
            | IdentityKa v -> $"kI{(toDoubleString v)}"
            | SeparableKa v ->
                let a = toModelString K0.defaultValue.value v.eeInfScale |> bindPrefix "k"
                let b = toModelStringArray KaFuncValue.defaultQuadraticCoefficients v.tEeInf.tEe.coefficients |> bindPrefix "_"
                let c = toModelStringArray KaFuncValue.defaultQuadraticCoefficients v.tEeInf.tInf.coefficients |> bindPrefix "i"
                let d = toModelStringArray (KaFuncValue.defaultQuadraticValue Domain2D.defaultValue).comparisonFactors k.comparisonFactors |> bindPrefix "@"
                let e = [| a; b; c; d |] |> Array.choose id |> joinStrings EmptyString
                e
            | SphericalKa v ->
                let a = toModelString K0.defaultValue.value v.scale |> bindPrefix "k"
                let b = toModelStringArray KaFuncValue.defaultQuadraticCoefficients v.taylorApproximation.coefficients |> bindPrefix "_"
                let d = toModelStringArray (KaFuncValue.defaultQuadraticValue Domain2D.defaultValue).comparisonFactors k.comparisonFactors |> bindPrefix "@"
                let e = [| a; b; d |] |> Array.choose id |> joinStrings EmptyString
                e


    /// gamma0 multiplier in gamma.
    type Gamma0 =
        | Gamma0 of double

        member r.value = let (Gamma0 v) = r in v
        static member defaultValue = Gamma0 0.01


    type GlobalAsymmetryFactor =
        | GlobalAsymmetryFactor of double

        member r.value = let (GlobalAsymmetryFactor v) = r in v
        static member defaultValue = GlobalAsymmetryFactor -0.01
        static member defaultSmallValueX5 = GlobalAsymmetryFactor -0.005
        static member defaultSmallValueX2 = GlobalAsymmetryFactor -0.002
        static member defaultSmallValue = GlobalAsymmetryFactor -0.001
        static member defaultVerySmallValue = GlobalAsymmetryFactor -0.0001


    type GammaFuncValue =
        | ScalarGamma of double
        | SeparableGamma of ScaledEeInfTaylorApproximation
        | SphericalGamma of ScaledTaylorApproximation

        member g.gammaFunc (_ : Domain2D) : GammaFunc =
            match g with
            | ScalarGamma e -> (fun _ _ _ -> e)
            | SeparableGamma e -> (fun _ a b -> e.eeInfScale * (separableFunc e.tEeInf a b))
            | SphericalGamma e -> fun _ a b -> e.scale * (e.taylorApproximation.calculate (radius a b))
            |> GammaFunc

        member g.gammaFuncUnscaled (_ : Domain2D) : GammaFunc =
            match g with
            | ScalarGamma _ -> (fun _ _ _ -> 1.0)
            | SeparableGamma e -> (fun _ a b -> separableFunc e.tEeInf a b)
            | SphericalGamma e -> fun _ a b -> e.taylorApproximation.calculate (radius a b)
            |> GammaFunc

        member g.gamma0 =
            match g with
            | ScalarGamma e -> e
            | SeparableGamma e -> e.eeInfScale
            | SphericalGamma e -> e.scale
            |> Gamma0

        member g.comparisonFactors =
            match g with
            | ScalarGamma _ -> [||]
            | SeparableGamma v -> v.comparisonFactors
            | SphericalGamma e -> e.comparisonFactors

        static member withGamma0 (gamma0 : Gamma0) (g : GammaFuncValue) =
            match g with
            | ScalarGamma _ -> ScalarGamma gamma0.value
            | SeparableGamma e -> SeparableGamma { e with eeInfScale = gamma0.value }
            | SphericalGamma e -> SphericalGamma { e with scale = gamma0.value }

        static member defaultValue = ScalarGamma Gamma0.defaultValue.value
        static member defaultNonLinearEeCoefficients = [| 1.0; GlobalAsymmetryFactor.defaultValue.value |]
        static member defaultNonLinearInfCoefficients = [| 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 1000.0 |]

        static member defaultNonLinearValue (d : Domain2D) =
            let tEe =
                {
                    x0 = 0.0
                    xScale = 1.0
                    coefficients = GammaFuncValue.defaultNonLinearEeCoefficients
                }

            let tInf =
                {
                    x0 = 0.0
                    xScale = twoThirdInfScale d
                    coefficients = GammaFuncValue.defaultNonLinearInfCoefficients
                }

            SeparableGamma { eeInfScale = Gamma0.defaultValue.value; tEeInf = { tEe = tEe; tInf = tInf } }

        static member defaultSymmetricNonLinearValue (d : Domain2D) =
            let ta =
                {
                    x0 = 0.0

                    // twoThirdInfScale assumes that the domain start from 0.0, which is not the case for spherical gamma.
                    xScale = 2.0 * (twoThirdInfScale d)
                    coefficients = GammaFuncValue.defaultNonLinearInfCoefficients
                }

            SphericalGamma { scale = Gamma0.defaultValue.value; taylorApproximation = ta }

        static member withGlobalAsymmetryFactor (a : GlobalAsymmetryFactor) (g : GammaFuncValue) =
            match g with
            | ScalarGamma _ -> failwith "Cannot set global asymmetry factor for scalar gamma."
            | SeparableGamma e -> SeparableGamma { e with tEeInf.tEe.coefficients = [| 1.0; a.value |] }
            | SphericalGamma _ -> failwith "Cannot set global asymmetry factor for spherical gamma."

        member g.modelString =
            match g with
            | ScalarGamma e -> $"gS{(toDoubleString e)}"
            | SeparableGamma e ->
                let a = toModelString Gamma0.defaultValue.value e.eeInfScale |> bindPrefix "g"
                let b = toModelStringArray GammaFuncValue.defaultNonLinearEeCoefficients e.tEeInf.tEe.coefficients |> bindPrefix "a"
                let c = toModelStringArray GammaFuncValue.defaultNonLinearInfCoefficients e.tEeInf.tInf.coefficients |> bindPrefix "_"
                let d = toModelStringArray (GammaFuncValue.defaultNonLinearValue Domain2D.defaultValue).comparisonFactors g.comparisonFactors |> bindPrefix "@"
                let e = [| a; b; c; d |] |> Array.choose id |> joinStrings EmptyString
                e.Replace("-", "") // We don't care about the sign as the only negative coefficient is asymmetry factor (due to historical reasons).
            | SphericalGamma e ->
                let a = toModelString Gamma0.defaultValue.value e.scale |> bindPrefix "g"
                let c = toModelStringArray GammaFuncValue.defaultNonLinearInfCoefficients e.taylorApproximation.coefficients |> bindPrefix "_"
                let d = toModelStringArray (GammaFuncValue.defaultNonLinearValue Domain2D.defaultValue).comparisonFactors g.comparisonFactors |> bindPrefix "@"
                let e = [| a; c; d |] |> Array.choose id |> joinStrings EmptyString
                e.Replace("-", "") // We don't care about the sign as the only negative coefficient is asymmetry factor (due to historical reasons).


    type MutationProbabilityParams =
        {
            domainParams : DomainParams
            zeroThreshold : ZeroThreshold
            epsFuncValue : EpsFuncValue
        }


    /// Creates a normalized mutation probability.
    /// The normalization is performed using integral estimate over the domain.
    type MutationProbability =
        | MutationProbability of SparseArray<double>

        member r.value = let (MutationProbability v) = r in v

        /// m is a real (unscaled) value but e is scaled to the half of the range.
        /// i is an index in the domain.
        static member create (e : EvolutionType) (data : MutationProbabilityParams) (i : int) =
            match e with
            | DifferentialEvolution ->
                let domain = data.domainParams.domain()
                let mean = domain.points.value[i]
                let ef = data.epsFuncValue.epsFunc domain
                let epsFunc x = (ef.invoke domain x) * domain.domainRange.range / 2.0
                let f x : double = exp (- pown ((x - mean) / (epsFunc x)) 2)
                let values = domain.points.value |> Array.map f |> SparseArray<double>.create data.zeroThreshold
                let norm = domain.integrateValues values
                let p = values.value |> Array.map (fun v -> { v with value1D = v.value1D / norm }) |> SparseArray.SparseArray |> MutationProbability
                p
            | DiscreteEvolution ->
                let domain = data.domainParams.domain()
                let ef = data.epsFuncValue.epsFunc domain
                let epsFunc (i1 : int) = (ef.invoke domain domain.points.value[i1]) * ( double domain.points.value.Length) / 2.0
                let f (i1 : int) : double = exp (- pown ((double (i1 - i)) / (epsFunc i1)) 2)
                let values = domain.points.value |> Array.mapi (fun i1 _ -> f i1) |> SparseArray<double>.create data.zeroThreshold
                let norm = values.total()
                let p = values.value |> Array.map (fun v -> { v with value1D = v.value1D / norm }) |> SparseArray.SparseArray |> MutationProbability
                p

    type MutationProbabilityParams2D =
        {
            eeMutationProbabilityParams : MutationProbabilityParams
            infMutationProbabilityParams : MutationProbabilityParams
        }

        member d.domain2D() =
            {
                d0 = d.eeMutationProbabilityParams.domainParams.domain()
                d1 = d.infMutationProbabilityParams.domainParams.domain()
            }


    type MutationProbability2D =
        | MutationProbability2D of SparseArray2D<double>

        member r.value = let (MutationProbability2D v) = r in v

        /// i and j are indices in the domain.
        static member create (e : EvolutionType) (data : MutationProbabilityParams2D) (i : int) (j : int) =
            let p1 = MutationProbability.create e data.eeMutationProbabilityParams i
            let p2 = MutationProbability.create e data.infMutationProbabilityParams j
            let p = cartesianMultiply p1.value p2.value // |> MutationProbability2D
            p


    /// Constructs p(x, y, x1, y1) sparse array where each numerical integral by (x, y) for each of (x1, y1) should be 1.
    /// Integration by (x, y) is "inconvenient" as normally we'd need to integrate by (x1, y1) and so the structure is
    /// optimized for that.
    type MutationProbability4D =
        {
            /// For integration over (x, y)
            x1y1_xy : SparseArray4D<double>

            /// For "standard" integration over (x1, y1)
            xy_x1y1 : SparseArray4D<double>
        }

        static member create (e : EvolutionType) (data : MutationProbabilityParams2D) =
            let domain2D = data.domain2D()
            let eeMu = domain2D.eeDomain.points.value
            let infMu = domain2D.infDomain.points.value

            // These are the values where integration by (x, y) should yield 1 for each (x1, y1).
            // So [][] is by (x1, y1) and the underlying SparseArray2D is by (x, y).
            let x1y1_xy = eeMu |> Array.mapi (fun i _ -> infMu |> Array.mapi (fun j _ -> MutationProbability2D.create e data i j))

            let xy_x1y1_Map =
                [| for i in 0..(eeMu.Length - 1) -> [| for j in 0..(infMu.Length - 1) -> SparseValue4D.createArray i j (x1y1_xy[i][j]) |] |]
                |> Array.concat
                |> Array.concat
                |> Array.groupBy (fun e -> e.i1, e.j1)
                |> Array.map (fun (a, b) -> a, b |> Array.map (fun e -> { i = e.i; j = e.j; value2D = e.value4D }) |> Array.sortBy (fun e -> e.i, e.j) |> SparseArray2D<double>.SparseArray2D)
                |> Map.ofArray

            let xy_x1y1 =
                eeMu
                |> Array.mapi (fun i _ -> infMu |> Array.mapi (fun j _ -> xy_x1y1_Map[(i, j)]))

            {
                x1y1_xy = SparseArray4D x1y1_xy
                xy_x1y1 = SparseArray4D xy_x1y1
            }


    type KernelParams =
        {
            domainIntervals : DomainIntervals
            eeInfDomainRange : DomainRange2D
            zeroThreshold : ZeroThreshold
            epsEeFuncValue : EpsFuncValue
            epsInfFuncValue : EpsFuncValue
            kaFuncValue : KaFuncValue
        }

        member kp.eeDomainParams =
            {
                domainIntervals = kp.domainIntervals
                domainRange = kp.eeInfDomainRange.r0
            }

        member kp.infDomainParams =
            {
                domainIntervals = kp.domainIntervals
                domainRange = kp.eeInfDomainRange.r1
            }

        member kp.mutationProbabilityData2D =
            {
                eeMutationProbabilityParams =
                    {
                        domainParams = kp.eeDomainParams
                        zeroThreshold = ZeroThreshold.defaultValue
                        epsFuncValue = kp.epsEeFuncValue
                    }
                infMutationProbabilityParams =
                    {
                        domainParams = kp.infDomainParams
                        zeroThreshold = ZeroThreshold.defaultValue
                        epsFuncValue = kp.epsInfFuncValue
                    }
            }

        member kp.domain2D() = Domain2D.create (kp.domainIntervals, kp.eeInfDomainRange)

        static member defaultValue =
            {
                domainIntervals = DomainIntervals.defaultValue
                eeInfDomainRange =
                    {
                        r0 = DomainRange.defaultValue
                        r1 = { minValue = 0.0; maxValue = InfMaxValue.defaultValue.value }
                    }
                zeroThreshold = ZeroThreshold.defaultValue
                epsEeFuncValue = EpsFuncValue.ScalarEps Eps0.defaultValue
                epsInfFuncValue = EpsFuncValue.ScalarEps Eps0.defaultValue
                kaFuncValue = KaFuncValue.IdentityKa 1.0
            }

        /// Same as above but with quadratic ka.
        static member defaultQuadraticValue =
            let kp = KernelParams.defaultValue
            { kp with kaFuncValue = KaFuncValue.defaultQuadraticValue (kp.domain2D()) }

        static member defaultSymmetricValue =
            {
                domainIntervals = DomainIntervals.defaultValue
                eeInfDomainRange = DomainRange2D.defaultValue
                zeroThreshold = ZeroThreshold.defaultValue
                epsEeFuncValue = EpsFuncValue.ScalarEps Eps0.defaultValue
                epsInfFuncValue = EpsFuncValue.ScalarEps Eps0.defaultValue
                kaFuncValue = KaFuncValue.IdentityKa 1.0
            }

        /// Same as above but with quadratic ka.
        static member defaultSymmetricQuadraticValue =
            let kp = KernelParams.defaultSymmetricValue
            { kp with kaFuncValue = KaFuncValue.defaultSymmetricQuadraticValue (kp.domain2D()) }

        static member defaultQuadraticWithLinearInfValue =
            let kp = KernelParams.defaultValue
            { kp with kaFuncValue = KaFuncValue.defaultQuadraticWithLinearInfValue (kp.domain2D()) }

        static member withEps0 (eps0 : Eps0) (kp : KernelParams) =
            { kp with epsEeFuncValue = EpsFuncValue.ScalarEps eps0; epsInfFuncValue = EpsFuncValue.ScalarEps eps0 }

        static member withK0 (k0 : K0) (kp : KernelParams) = { kp with kaFuncValue = kp.kaFuncValue.withK0 k0 }
        static member withKaFunc (ka : KaFuncValue) (kp : KernelParams) = { kp with kaFuncValue = ka }
        static member withDomainIntervals (d : DomainIntervals) (kp : KernelParams) = { kp with domainIntervals = d }


    type Ka =
        | Ka of Matrix<double>

        member r.value = let (Ka v) = r in v


    type Kernel =
        | Kernel of SparseArray4D<double>

        member r.value = let (Kernel v) = r in v


    /// Represents K(x, x1, y, y1) 2D Fredholm kernel.
    /// It is convenient to store it in a form,
    /// where the first two indexes are (x, y) and last ones are (x1, y1).
    /// So the actual indexes here are K(x, y, x1, y1).
    type KernelData =
        {
            kernel : Kernel
            ka : Ka
            domain2D : Domain2D
        }

        member k.integrateValues (u : LinearMatrix<double>) =
            let v = k.domain2D.integrateValues (k.kernel.value, u)
            v

        /// Calculates how many protocells are created.
        /// The multiplier carries the value in front of the integral (f^n).
        member k.evolve (useParallel : bool) (p : PoissonSampler) (multiplier : double) (u : Matrix<int64>) =
            let v = k.domain2D.evolve (useParallel, p, multiplier, k.kernel.value, u)
            v

        static member create (e : EvolutionType) p =
            let domain2D = Domain2D.create (p.domainIntervals, p.eeInfDomainRange)

            let mp2 =
                {
                    eeMutationProbabilityParams =
                        {
                            domainParams = p.eeDomainParams
                            zeroThreshold = p.zeroThreshold
                            epsFuncValue = p.epsEeFuncValue
                        }

                    infMutationProbabilityParams =
                        {
                            domainParams = p.infDomainParams
                            zeroThreshold = p.zeroThreshold
                            epsFuncValue = p.epsInfFuncValue
                        }
                }

            let mp4 = MutationProbability4D.create e mp2
            let kaFunc = p.kaFuncValue.kaFunc domain2D

            let ka =
                domain2D.eeDomain.points.value
                |> Array.map (fun x -> domain2D.infDomain.points.value |> Array.map (fun y -> kaFunc.invoke domain2D x y))
                |> Matrix

            let kernel = mp4.xy_x1y1 * ka |> Kernel

            {
                kernel = kernel
                ka = Ka ka
                domain2D = domain2D
            }

    type Gamma =
        | Gamma of Matrix<double>

        member r.value = let (Gamma v) = r in v

        /// Calculates how many protocells are destroyed.
        member r.evolve (p : PoissonSingleSampler) (u : Matrix<int64>) =
            let m = r.value.value

            let g i j v =
                if v <= 0L then 0L
                else
                    let lambda = (double v) * m[i][j]
                    let retVal = p.nextPoisson lambda
                    min retVal v // Cannot destroy more than we have.

            let retVal = u.value |> Array.mapi (fun i a -> a |> Array.mapi (fun j b -> g i j b)) |> Matrix
            retVal

        static member create (d : Domain2D) (g : GammaFuncValue) : Gamma =
            let gamma =
                d.eeDomain.points.value
                |> Array.map (fun a -> d.infDomain.points.value |> Array.map (fun b -> (g.gammaFunc d).invoke d a b))
                |> Matrix
                |> Gamma

            gamma


    type RecyclingRate
        with

        member w.modelString =
            toModelString RecyclingRate.defaultValue.value w.value
            |> bindPrefix "w"
            |> Option.defaultValue EmptyString

        /// Calculates how much waste is recycled.
        member r.evolve (p : PoissonSingleSampler) w =
            if w <= 0L then 0L
            else
                let lambda = r.value * (double w)
                let retVal = p.nextPoisson lambda
                min retVal w // Cannot recycle more than we have.


    /// Common parameters between differential and Poisson based models.
    type EeInfModelParams =
        {
            kernelParams : KernelParams
            gammaFuncValue : GammaFuncValue
            numberOfMolecules : NumberOfMolecules
            recyclingRate : RecyclingRate
            name : string
        }

        member mp.modelString =
            let domain = mp.kernelParams.domain2D()

            [|
                domain.modelString
                mp.kernelParams.kaFuncValue.modelString
                mp.kernelParams.epsEeFuncValue.modelString
                mp.gammaFuncValue.modelString
                mp.recyclingRate.modelString
            |]
            |> joinStrings EmptyString

        /// Default linear value, mostly for tests, as it does not have many practical purposes.
        static member defaultValue =
            {
                kernelParams = KernelParams.defaultValue
                gammaFuncValue = GammaFuncValue.defaultValue
                numberOfMolecules = NumberOfMolecules.defaultValue
                recyclingRate = RecyclingRate.defaultValue
                name = EmptyString
            }

        static member defaultSymmetricValue =
            {
                kernelParams = KernelParams.defaultSymmetricValue
                gammaFuncValue = GammaFuncValue.defaultValue
                numberOfMolecules = NumberOfMolecules.defaultValue
                recyclingRate = RecyclingRate.defaultValue
                name = EmptyString
            }

        /// Default value with quadratic kernel and non-linear gamma.
        /// This is the main starting point where we can vary k0, eps0, gamma0, etc...
        static member defaultNonLinearValue =
            let kp = KernelParams.defaultQuadraticValue
            let d = kp.domain2D()
            { EeInfModelParams.defaultValue with kernelParams = kp; gammaFuncValue = GammaFuncValue.defaultNonLinearValue d }

        /// Default SYMMETRIC value with quadratic kernel and non-linear gamma.
        /// This is the main starting point where we can vary k0, eps0, gamma0, etc...
        static member defaultSymmetricNonLinearValue =
            let kp = KernelParams.defaultSymmetricQuadraticValue
            let d = kp.domain2D()
            { EeInfModelParams.defaultSymmetricValue with kernelParams = kp; gammaFuncValue = GammaFuncValue.defaultSymmetricNonLinearValue d }

        static member defaultQuadraticWithLinearInfValue =
            let kp = KernelParams.defaultQuadraticWithLinearInfValue
            let d = kp.domain2D()
            { EeInfModelParams.defaultValue with kernelParams = kp; gammaFuncValue = GammaFuncValue.defaultNonLinearValue d }

        static member withK0 k0 p = { p with kernelParams = p.kernelParams |> KernelParams.withK0 k0 }
        static member withKaFunc (ka : KaFuncValue) p = { p with kernelParams = p.kernelParams |> KernelParams.withKaFunc ka }
        static member withEps0 eps0 p = { p with kernelParams = p.kernelParams |> KernelParams.withEps0 eps0 }
        static member withGamma0 gamma0 p = { p with gammaFuncValue = p.gammaFuncValue |> GammaFuncValue.withGamma0 gamma0 }
        static member withDomainIntervals d p = { p with kernelParams = p.kernelParams |> KernelParams.withDomainIntervals d }
        static member withInfMaxValue (InfMaxValue infMaxValue) p = { p with kernelParams.eeInfDomainRange.r1.maxValue = infMaxValue }
        static member withGlobalAsymmetryFactor a p = { p with gammaFuncValue = p.gammaFuncValue |> GammaFuncValue.withGlobalAsymmetryFactor a }
