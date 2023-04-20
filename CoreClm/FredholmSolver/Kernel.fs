namespace FredholmSolver

open System
open FredholmSolver.Primitives

module Kernel =


    /// Describes a function domain suitable for integral approximation.
    /// Equidistant grid is used to reduce the number of multiplications.
    type Domain =
        {
            midPoints : Vector<double>
            step : double
            range : double
        }

        member d.noOfIntervals = d.midPoints.value.Length

        member d.integrateValues (v : SparseArray<double>) =
            let sum = v.value |> Array.map (fun e -> e.value1D) |> Array.sum
            sum * d.step

        static member create noOfIntervals minValue maxValue =
            let points = [| for i in 0..noOfIntervals -> minValue + (maxValue - minValue) * (double i) / (double noOfIntervals) |]

            {
                midPoints = [| for i in 0..noOfIntervals - 1 -> (points[i + 1] + points[i]) / 2.0 |] |> Vector
                step =  (maxValue - minValue) / (double noOfIntervals)
                range = points[noOfIntervals] - points[0]
            }


    /// Number of intervals in the domain.
    type DomainIntervals =
        | DomainIntervals of int

        member r.value = let (DomainIntervals v) = r in v

        static member defaultValue = DomainIntervals 101


    type DomainRange =
        {
            minValue : double
            maxValue : double
        }


    type DomainData =
        {
            domainIntervals : DomainIntervals
            domainRange : DomainRange
        }

        member dd.domain() =
            Domain.create dd.domainIntervals.value dd.domainRange.minValue dd.domainRange.maxValue


    /// Data that describes a rectangle in ee * inf space.
    /// ee space is naturally limited to [-1, 1] unless we use a conformal transformation to extend it to [-Infinity, Infinity].
    /// inf (information) space is naturally limited at the lower bound (0). The upper bound can be rescaled to any number or even taken to Infinity.
    type Domain2D =
        {
            eeDomain : Domain
            infDomain : Domain
        }

        member private d.normalize v = v * d.eeDomain.step * d.infDomain.step
        member private d.integrateValues (a : double[][]) = a |> Array.map (fun e -> e |> Array.sum) |> Array.sum |> d.normalize
        member private d.integrateValues (a : SparseArray2D<double>) = a.value |> Array.map (fun e -> e.value2D) |> Array.sum |> d.normalize

        member d.integrateValues (a : Matrix<double>) =
            let sum = a.value |> Array.map (fun e -> e |> Array.sum) |> Array.sum |> d.normalize
            sum

        member d.integrateValues (a : LinearMatrix<double>) =
            let sum = a.d1Range |> Array.map (fun i -> a.d2Range |> Array.map (fun j -> a.getValue i j) |> Array.sum) |> Array.sum |> d.normalize
            sum

        member private d.integrateValues (a : SparseArray2D<double>, b : LinearMatrix<double>) =
            let bValue = b.getValue
            let sum = a.value |> Array.map (fun e -> e.value2D * (bValue e.i e.j)) |> Array.sum |> d.normalize
            sum

        member d.integrateValues (a : SparseArray4D<double>, b : LinearMatrix<double>) =
            a.value |> Array.map (fun v -> v |> Array.map (fun e -> d.integrateValues (e, b))) |> Matrix

        member d.integrateValues (a : SparseArray4D<double>) =
            a.value |> Array.map (fun v -> v |> Array.map d.integrateValues) |> Matrix

        member d.norm (a : LinearMatrix<double>) = d.integrateValues a

        member d.mean (a : LinearMatrix<double>) =
            let norm = d.norm a

            if norm > 0.0
            then
                let mx = (d.integrateValues (d.eeDomain.midPoints * a)) / norm
                let my = (d.integrateValues (a * d.infDomain.midPoints)) / norm
                (mx, my)
            else (0.0, 0.0)

        member d.stdDev (a : LinearMatrix<double>) =
            let norm = d.norm a

            if norm > 0.0
            then
                let mx, my = d.mean a
                let m2x = (d.integrateValues (d.eeDomain.midPoints * (d.eeDomain.midPoints * a))) / norm
                let m2y = (d.integrateValues ((a * d.infDomain.midPoints) * d.infDomain.midPoints)) / norm
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
                // midPoints = cartesianMultiply eeDomain infDomain
            }


    /// Type to describe a function used to calculate eps in mutation probability calculations.
    type EpsFunc =
        | EpsFunc of (Domain -> double -> double)

        member r.invoke = let (EpsFunc v) = r in v


    /// ka portion of the kernel.
    type KaFunc =
        | KaFunc of (Domain2D -> double -> double -> double)

        member r.invoke = let (KaFunc v) = r in v


    /// Decay function.
    type GammaFunc =
        | GammaFunc of (Domain2D -> double -> double -> double)

        member r.invoke = let (GammaFunc v) = r in v


    type EpsFuncValue =
        | ScalarEps of double

        member ef.epsFunc (d : Domain) : EpsFunc =
            match ef with
            | ScalarEps e -> EpsFunc (fun _ _ -> e)


    type KaFuncValue =
        | IdentityKa
        | QuadraticSeparableKa
        | QuadraticInseparableKa

        member k.kaFunc (d : Domain2D) : KaFunc =
            match k with
            | IdentityKa -> (fun _ _ _ -> 1.0)
            | QuadraticSeparableKa -> (fun _ _ _ -> 1.0)
            | QuadraticInseparableKa -> (fun _ _ _ -> 1.0)
            |> KaFunc


    type GammaFuncValue =
        | ScalarGamma of double

        member g.gammaFunc (d : Domain2D) : GammaFunc =
            match g with
            | ScalarGamma e -> GammaFunc (fun _ _ _ -> e)

        static member defaultValue = ScalarGamma 0.01


    type MutationProbabilityData =
        {
            domainData : DomainData
            zeroThreshold : ZeroThreshold
            epsFuncValue : EpsFuncValue
        }


    /// Creates a normalized mutation probability.
    /// The normalization is performed using integral estimate over the domain.
    type MutationProbability =
        | MutationProbability of SparseArray<double>

        member r.value = let (MutationProbability v) = r in v

        /// m is a real (unscaled) value but e is scaled to the half of the range.
        static member create (data : MutationProbabilityData) mean =
            let domain = data.domainData.domain()
            let ef = data.epsFuncValue.epsFunc domain
            let epsFunc x = (ef.invoke domain x) * domain.range / 2.0
            let f x : double = exp (- pown ((x - mean) / (epsFunc x)) 2)
            let values = domain.midPoints.value |> Array.map f |> SparseArray<double>.create data.zeroThreshold
            let norm = domain.integrateValues values
            let p = values.value |> Array.map (fun v -> { v with value1D = v.value1D / norm }) |> SparseArray.SparseArray |> MutationProbability
            p


    type MutationProbabilityData2D =
        {
            eeMutationProbabilityData : MutationProbabilityData
            infMutationProbabilityData : MutationProbabilityData
        }

        member d.domain2D() =
            {
                eeDomain = d.eeMutationProbabilityData.domainData.domain()
                infDomain = d.infMutationProbabilityData.domainData.domain()
            }


    type MutationProbability2D =
        | MutationProbability2D of SparseArray2D<double>

        member r.value = let (MutationProbability2D v) = r in v

        static member create (data : MutationProbabilityData2D) eeMean infMean =
            let p1 = MutationProbability.create data.eeMutationProbabilityData eeMean
            let p2 = MutationProbability.create data.infMutationProbabilityData infMean
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

        static member create (data : MutationProbabilityData2D) =
            let domain2D = data.domain2D()
            let eeMu = domain2D.eeDomain.midPoints.value
            let infMu = domain2D.infDomain.midPoints.value

            // These are the values where integration by (x, y) should yield 1 for each (x1, y1).
            // So [][] is by (x1, y1) and the underlying SparseArray2D is by (x, y).
            let x1y1_xy = eeMu |> Array.map (fun a -> infMu |> Array.map (MutationProbability2D.create data a))

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


    type KernelData =
        {
            domainIntervals : DomainIntervals
            infMaxValue : InfMaxValue
            zeroThreshold : ZeroThreshold
            epsEeFuncValue : EpsFuncValue
            epsInfFuncValue : EpsFuncValue
            kaFuncValue : KaFuncValue
        }

        member kd.eeDomainData =
            {
                domainIntervals = kd.domainIntervals
                domainRange =
                    {
                        minValue = Domain2D.eeMinValue
                        maxValue = Domain2D.eeMaxValue
                    }
            }

        member kd.infDomainData =
            {
                domainIntervals = kd.domainIntervals
                domainRange =
                    {
                        minValue = Domain2D.infDefaultMinValue
                        maxValue = kd.infMaxValue.value
                    }
            }

        member kd.mutationProbabilityData2D =
            {
                eeMutationProbabilityData =
                    {
                        domainData = kd.eeDomainData
                        zeroThreshold = ZeroThreshold.defaultValue
                        epsFuncValue = kd.epsEeFuncValue
                    }
                infMutationProbabilityData =
                    {
                        domainData = kd.infDomainData
                        zeroThreshold = ZeroThreshold.defaultValue
                        epsFuncValue = kd.epsInfFuncValue
                    }
            }


        static member defaultValue =
            {
                domainIntervals = DomainIntervals.defaultValue
                infMaxValue = InfMaxValue.defaultValue
                zeroThreshold = ZeroThreshold.defaultValue
                epsEeFuncValue = EpsFuncValue.ScalarEps 0.02
                epsInfFuncValue = EpsFuncValue.ScalarEps 0.02
                kaFuncValue = KaFuncValue.IdentityKa
            }

        static member defaultNarrowValue =
            {
                domainIntervals = DomainIntervals.defaultValue
                infMaxValue = InfMaxValue.defaultValue
                zeroThreshold = ZeroThreshold.defaultValue
                epsEeFuncValue = EpsFuncValue.ScalarEps 0.0001
                epsInfFuncValue = EpsFuncValue.ScalarEps 0.0001
                kaFuncValue = KaFuncValue.IdentityKa
            }


    type KernelValue =
        | KernelValue of SparseArray4D<double>

        member r.value = let (KernelValue v) = r in v


    /// Represents K(x, x1, y, y1) 2D Fredholm kernel.
    /// It is convenient to store it as in a form,
    /// where the first two indexes are (x, y) and last ones are (x1, y1).
    /// So the actual indexes here are K(x, y, x1, y1).
    type Kernel =
        {
            kernel : KernelValue
            domain2D : Domain2D
        }

        member k.integrateValues (u : LinearMatrix<double>) =
            let v = k.domain2D.integrateValues (k.kernel.value, u)
            v

        static member create data =
            let domain2D = Domain2D.create data.domainIntervals.value data.infMaxValue.value

            let mpData =
                {
                    eeMutationProbabilityData =
                        {
                            domainData = data.eeDomainData
                            zeroThreshold = data.zeroThreshold
                            epsFuncValue = data.epsEeFuncValue
                        }

                    infMutationProbabilityData =
                        {
                            domainData = data.infDomainData
                            zeroThreshold = data.zeroThreshold
                            epsFuncValue = data.epsInfFuncValue
                        }
                }

            let mp = MutationProbability4D.create mpData
            let kaFunc = data.kaFuncValue.kaFunc domain2D

            let ka =
                domain2D.eeDomain.midPoints.value
                |> Array.map (fun x -> domain2D.infDomain.midPoints.value |> Array.map (fun y -> kaFunc.invoke domain2D x y))
                |> Matrix

            let kernel = mp.xy_x1y1 * ka |> KernelValue

            {
                kernel = kernel
                domain2D = domain2D
            }

    type Gamma =
        | Gamma of Matrix<double>

        member r.value = let (Gamma v) = r in v

        static member create (d : Domain2D) (g : GammaFuncValue) : Gamma =
            let gamma =
                d.eeDomain.midPoints.value
                |> Array.map (fun a -> d.infDomain.midPoints.value |> Array.map (fun b -> (g.gammaFunc d).invoke d a b))
                |> Matrix
                |> Gamma

            gamma


    /// Number of "molecules" or building blocks used in a protocell.
    type NumberOfMolecules =
        | NumberOfMolecules of int

        member r.value = let (NumberOfMolecules v) = r in v
        static member defaultValue = NumberOfMolecules 100


    type RecyclingRate =
        | RecyclingRate of double

        member r.value = let (RecyclingRate v) = r in v
        static member defaultValue = RecyclingRate 1.0
