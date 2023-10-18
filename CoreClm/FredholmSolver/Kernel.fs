namespace FredholmSolver

open System
open FredholmSolver.Primitives

module Kernel =

    /// Describes a function domain suitable for integral approximation.
    /// Equidistant grid is used to reduce the number of multiplications.
    type Domain =
        {
            points : Vector<double>
            step : double
            range : double
        }

        member private d.integralValue xSize (v : SparseValue<double>) =
            match v.i = 0, v.i = xSize with
            | true, false -> 0.5 * v.value1D
            | false, true -> 0.5 * v.value1D
            | _ -> v.value1D

        member d.noOfIntervals = d.points.value.Length - 1

        member d.integrateValues (v : SparseArray<double>) =
            let len = d.noOfIntervals
            let sum = v.value |> Array.map (fun e -> d.integralValue len e) |> Array.sum
            sum * d.step

        /// Number of points is (noOfIntervals + 1).
        static member create noOfIntervals minValue maxValue =
            let points = [| for i in 0..noOfIntervals -> minValue + (maxValue - minValue) * (double i) / (double noOfIntervals) |]

            {
                points = Vector points
                step = (maxValue - minValue) / (double noOfIntervals)
                range = points[noOfIntervals] - points[0]
            }


    /// Number of intervals in the domain.
    type DomainIntervals =
        | DomainIntervals of int

        member r.value = let (DomainIntervals v) = r in v
        static member defaultValue = DomainIntervals 100


    type DomainRange =
        {
            minValue : double
            maxValue : double
        }


    type DomainParams =
        {
            domainIntervals : DomainIntervals
            domainRange : DomainRange
        }

        member dd.domain() =
            Domain.create dd.domainIntervals.value dd.domainRange.minValue dd.domainRange.maxValue


    /// Data that describes a rectangle in ee * inf space.
    /// ee space is naturally limited to [-1, 1] unless we use a conformal-like transformation to extend it to [-Infinity, Infinity].
    /// inf (information) space is naturally limited at the lower bound (0). The upper bound can be rescaled to any number or even taken to Infinity.
    type Domain2D =
        {
            eeDomain : Domain
            infDomain : Domain
        }

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
        member private d.evolve(p : PoissonSampler, multiplier : double, a : SparseArray2D<double>, b : Matrix<int64>) =
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
        member private d.evolve(p : PoissonSampler, a : SparseArray2D<double>, b : LinearMatrix<int64>) =
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

        /// Calculates how many protocells are created.
        member d.evolve (p : PoissonSampler, multiplier : double, a : SparseArray4D<double>, b : Matrix<int64>) =
            a.value |> Array.map (fun v -> v |> Array.map (fun e -> d.evolve (p, multiplier, e, b))) |> Matrix

        /// Calculates how many protocells are created.
        member d.evolve (p : PoissonSampler, a : SparseArray4D<double>, b : LinearMatrix<int64>) =
            a.value |> Array.map (fun v -> v |> Array.map (fun e -> d.evolve (p, e, b))) |> Matrix

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

        static member eeMinValue = -1.0
        static member eeMaxValue = 1.0
        static member infDefaultMinValue = 0.0

        static member create noOfIntervals l2 =
            let eeDomain = Domain.create noOfIntervals Domain2D.eeMinValue Domain2D.eeMaxValue
            let infDomain = Domain.create noOfIntervals Domain2D.infDefaultMinValue l2

            {
                eeDomain = eeDomain
                infDomain = infDomain
            }


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


    type EeInfTaylorApproximation =
        {
            tEe : TaylorApproximation
            tInf : TaylorApproximation
        }


    type ScaledEeInfTaylorApproximation =
        {
            tEeInf : EeInfTaylorApproximation
            eeInfScale : double
        }


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


    type EpsFuncValue =
        | ScalarEps of double

        member ef.epsFunc (_ : Domain) : EpsFunc =
            match ef with
            | ScalarEps e -> EpsFunc (fun _ _ -> e)


    let separableFunc (v : EeInfTaylorApproximation) a b =
        let eeVal = v.tEe.calculate a
        let infVal = v.tInf.calculate b
        eeVal * infVal


    /// We want (2 / 3) of the domain range to scale to 1.0.
    let twoThirdInfScale (d : Domain2D) =
        let one = (2.0 / 3.0) * d.infDomain.range
        let scale = 1.0 / one
        scale


    type KaFuncValue =
        | IdentityKa of double
        | SeparableKa of ScaledEeInfTaylorApproximation

        member k.kaFunc (_ : Domain2D) : KaFunc =
            match k with
            | IdentityKa v -> (fun _ _ _ -> v)
            | SeparableKa v -> (fun _ a b -> v.eeInfScale * (separableFunc v.tEeInf a b))
            |> KaFunc

        /// Same as kaFunc but without k0.
        member k.kaFuncUnscaled (_ : Domain2D) : KaFunc =
            match k with
            | IdentityKa _ -> (fun _ _ _ -> 1.0)
            | SeparableKa v -> (fun _ a b -> separableFunc v.tEeInf a b)
            |> KaFunc

        /// k0 - effective y scale of ka.
        member k.k0 =
            match k with
            | IdentityKa v -> v
            | SeparableKa v -> v.eeInfScale
            |> K0

        /// Changes k0 value.
        member k.withK0 (k0 : K0) =
            match k with
            | IdentityKa _ -> IdentityKa k0.value
            | SeparableKa v -> SeparableKa { v with eeInfScale = k0.value }

        /// Default value is 1.0 on all domain.
        static member defaultValue = IdentityKa 1.0

        /// Quadratic growth from (0, 0) point.
        static member defaultQuadraticValue (d : Domain2D) =
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
                    coefficients = [| 1.0; 0.0; 1.0 |]
                }

            SeparableKa { eeInfScale = K0.defaultSmallValue.value; tEeInf = { tEe = tEe; tInf = tInf } }

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

            SeparableKa { eeInfScale = K0.defaultSmallValue.value; tEeInf = { tEe = tEe; tInf = tInf } }

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

            SeparableKa { eeInfScale = K0.defaultSmallValue.value; tEeInf = { tEe = tEe; tInf = tInf } }

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


    /// gamma0 multiplier in gamma.
    type Gamma0 =
        | Gamma0 of double

        member r.value = let (Gamma0 v) = r in v
        static member defaultValue = Gamma0 0.01


    type GlobalAsymmetryFactor =
        | GlobalAsymmetryFactor of double

        member r.value = let (GlobalAsymmetryFactor v) = r in v
        static member defaultValue = GlobalAsymmetryFactor -0.01
        static member defaultSmallValue = GlobalAsymmetryFactor -0.001
        static member defaultVerySmallValue = GlobalAsymmetryFactor -0.0001


    type GammaFuncValue =
        | ScalarGamma of double
        | SeparableGamma of ScaledEeInfTaylorApproximation

        member g.gammaFunc (_ : Domain2D) : GammaFunc =
            match g with
            | ScalarGamma e -> (fun _ _ _ -> e)
            | SeparableGamma e -> (fun _ a b -> e.eeInfScale * (separableFunc e.tEeInf a b))
            |> GammaFunc

        member g.gammaFuncUnscaled (_ : Domain2D) : GammaFunc =
            match g with
            | ScalarGamma _ -> (fun _ _ _ -> 1.0)
            | SeparableGamma e -> (fun _ a b -> separableFunc e.tEeInf a b)
            |> GammaFunc

        member g.gamma0 =
            match g with
            | ScalarGamma e -> e
            | SeparableGamma e -> e.eeInfScale
            |> Gamma0

        static member withGamma0 (gamma0 : Gamma0) (g : GammaFuncValue) =
            match g with
            | ScalarGamma _ -> ScalarGamma gamma0.value
            | SeparableGamma e -> SeparableGamma { e with eeInfScale = gamma0.value }

        static member defaultValue = ScalarGamma Gamma0.defaultValue.value

        static member defaultNonLinearValue (d : Domain2D) =
            let tEe =
                {
                    x0 = 0.0
                    xScale = 1.0
                    coefficients = [| 1.0; GlobalAsymmetryFactor.defaultValue.value |]
                }

            let tInf =
                {
                    x0 = 0.0
                    xScale = twoThirdInfScale d
                    coefficients = [| 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 1000.0 |]
                }

            SeparableGamma { eeInfScale = Gamma0.defaultValue.value; tEeInf = { tEe = tEe; tInf = tInf } }

        static member withGlobalAsymmetryFactor (a : GlobalAsymmetryFactor) (g : GammaFuncValue) =
            match g with
            | ScalarGamma _ -> failwith "Cannot set global asymmetry factor for scalar gamma."
            | SeparableGamma e -> SeparableGamma { e with tEeInf = { e.tEeInf with tEe = { e.tEeInf.tEe with coefficients = [| 1.0; a.value |] } } }


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
                let mean = domain.points.value.[i]
                let ef = data.epsFuncValue.epsFunc domain
                let epsFunc x = (ef.invoke domain x) * domain.range / 2.0
                let f x : double = exp (- pown ((x - mean) / (epsFunc x)) 2)
                let values = domain.points.value |> Array.map f |> SparseArray<double>.create data.zeroThreshold
                let norm = domain.integrateValues values
                let p = values.value |> Array.map (fun v -> { v with value1D = v.value1D / norm }) |> SparseArray.SparseArray |> MutationProbability
                p
            | DiscreteEvolution ->
                let domain = data.domainParams.domain()
                let ef = data.epsFuncValue.epsFunc domain
                let epsFunc (i1 : int) = (ef.invoke domain domain.points.value.[i1]) * ( double domain.points.value.Length) / 2.0
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
                eeDomain = d.eeMutationProbabilityParams.domainParams.domain()
                infDomain = d.infMutationProbabilityParams.domainParams.domain()
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


    /// Max value of the range in inf space.
    type InfMaxValue =
        | InfMaxValue of double

        member r.value = let (InfMaxValue v) = r in v
        static member defaultValue = InfMaxValue 25.0


    type KernelParams =
        {
            domainIntervals : DomainIntervals
            infMaxValue : InfMaxValue
            zeroThreshold : ZeroThreshold
            epsEeFuncValue : EpsFuncValue
            epsInfFuncValue : EpsFuncValue
            kaFuncValue : KaFuncValue
        }

        member kp.eeDomainParams =
            {
                domainIntervals = kp.domainIntervals
                domainRange =
                    {
                        minValue = Domain2D.eeMinValue
                        maxValue = Domain2D.eeMaxValue
                    }
            }

        member kp.infDomainParams =
            {
                domainIntervals = kp.domainIntervals
                domainRange =
                    {
                        minValue = Domain2D.infDefaultMinValue
                        maxValue = kp.infMaxValue.value
                    }
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

        member kp.domain2D() = Domain2D.create kp.domainIntervals.value kp.infMaxValue.value

        static member defaultValue =
            {
                domainIntervals = DomainIntervals.defaultValue
                infMaxValue = InfMaxValue.defaultValue
                zeroThreshold = ZeroThreshold.defaultValue
                epsEeFuncValue = EpsFuncValue.ScalarEps Eps0.defaultValue.value
                epsInfFuncValue = EpsFuncValue.ScalarEps Eps0.defaultValue.value
                kaFuncValue = KaFuncValue.IdentityKa 1.0
            }

        /// Same as above but with quadratic ka.
        static member defaultQuadraticValue =
            let kp = KernelParams.defaultValue
            { kp with kaFuncValue = KaFuncValue.defaultQuadraticValue (kp.domain2D()) }

        static member defaultQuadraticWithLinearInfValue =
            let kp = KernelParams.defaultValue
            { kp with kaFuncValue = KaFuncValue.defaultQuadraticWithLinearInfValue (kp.domain2D()) }

        static member withEps0 (eps0 : Eps0) (kp : KernelParams) =
            { kp with epsEeFuncValue = EpsFuncValue.ScalarEps eps0.value; epsInfFuncValue = EpsFuncValue.ScalarEps eps0.value }

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
        member k.evolve (p : PoissonSampler) (multiplier : double) (u : Matrix<int64>) =
            let v = k.domain2D.evolve (p, multiplier, k.kernel.value, u)
            v

        static member create (e : EvolutionType) p =
            let domain2D = Domain2D.create p.domainIntervals.value p.infMaxValue.value

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
        member r.evolve (p : PoissonSampler) (u : Matrix<int64>) =
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


    /// Number of "molecules" or building blocks used in a protocell.
    /// This controls the non linearity of the creation model.
    /// Default value is set to 1 because we take into account that a single protocell encounters with food
    /// proportionally to concentration of the food.
    type NumberOfMolecules =
        | NumberOfMolecules of int

        member r.value = let (NumberOfMolecules v) = r in v
        static member defaultValue = NumberOfMolecules 1
        static member defaultValue2 = NumberOfMolecules 2


    type RecyclingRate =
        | RecyclingRate of double

        member r.value = let (RecyclingRate v) = r in v
        static member defaultValue = RecyclingRate 1.0

        /// Calculates how much waste is recycled.
        member r.evolve (p : PoissonSampler) w =
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
            name : string option
        }

        /// Default linear value, mostly for tests, as it does not have many practical purposes.
        static member defaultValue =
            {
                kernelParams = KernelParams.defaultValue
                gammaFuncValue = GammaFuncValue.defaultValue
                numberOfMolecules = NumberOfMolecules.defaultValue
                recyclingRate = RecyclingRate.defaultValue
                name = None
            }

        /// Default value with quadratic kernel and non-linear gamma.
        /// This is the main starting point where we can vary k0, eps0, gamma0, etc...
        static member defaultNonLinearValue =
            let kp = KernelParams.defaultQuadraticValue
            let d = kp.domain2D()
            { EeInfModelParams.defaultValue with kernelParams = kp; gammaFuncValue = GammaFuncValue.defaultNonLinearValue d }

        static member defaultQuadraticWithLinearInfValue =
            let kp = KernelParams.defaultQuadraticWithLinearInfValue
            let d = kp.domain2D()
            { EeInfModelParams.defaultValue with kernelParams = kp; gammaFuncValue = GammaFuncValue.defaultNonLinearValue d }

        static member withK0 k0 p = { p with kernelParams = p.kernelParams |> KernelParams.withK0 k0 }
        static member withKaFunc (ka : KaFuncValue) p = { p with kernelParams = p.kernelParams |> KernelParams.withKaFunc ka }
        static member withEps0 eps0 p = { p with kernelParams = p.kernelParams |> KernelParams.withEps0 eps0 }
        static member withGamma0 gamma0 p = { p with gammaFuncValue = p.gammaFuncValue |> GammaFuncValue.withGamma0 gamma0 }
        static member withDomainIntervals d p = { p with kernelParams = p.kernelParams |> KernelParams.withDomainIntervals d }
        static member withInfMaxValue infMaxValue p = { p with kernelParams = { p.kernelParams with infMaxValue = infMaxValue } }
        static member withGlobalAsymmetryFactor a p = { p with gammaFuncValue = p.gammaFuncValue |> GammaFuncValue.withGlobalAsymmetryFactor a }
