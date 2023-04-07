namespace FredholmRunner

open FSharp.Collections

module FredholmData =

    /// TODO kk:20230402 - Exists in Gillespie.SsaPrimitives - Consolidate.
    type EnantiomericExcess =
        | EnantiomericExcess of double

        member r.value = let (EnantiomericExcess v) = r in v


    /// Representation of 2D values.
    /// It is convenient to store them as [][] rather than as [,] to leverage build in F# array manipulations.
    type XY =
        | XY of double[][]

        member r.value = let (XY v) = r in v


    /// Representation of non-zero value in a sparse array.
    type SparseValue =
        {
            i : int
            value : double
        }


    type SparseArray =
        | SparseArray of SparseValue[]

        member r.value = let (SparseArray v) = r in v

        static member create z v =
            v
            |> Array.mapi (fun i e -> if e >= z then Some { i = i; value = e } else None)
            |> Array.choose id
            |> SparseArray


    type SparseValue2D =
        {
            i : int
            j : int
            value : double
        }


    type SparseArray2D =
        | SparseArray2D of SparseValue2D[]

        member r.value = let (SparseArray2D v) = r in v

        static member create z v =
            v
            |> Array.mapi (fun i e -> e |> Array.mapi (fun j v -> if v >= z then Some { i = i; j = j; value = v } else None))
            |> Array.concat
            |> Array.choose id
            |> SparseArray2D


    /// Performs a Cartesian multiplication of two 1D sparse arrays to obtain a 2D sparse array.
    let cartesianMultiply (a : SparseArray) (b : SparseArray) : SparseArray2D =
        let bValue = b.value
        a.value |> Array.map (fun e -> bValue |> Array.map (fun f -> { i = e.i; j = f.i; value = e.value * f.value })) |> Array.concat |> SparseArray2D


    /// Describes a function domain suitable for integral approximation.
    /// Equidistant grid is used to reduce the number of multiplications.
    type Domain =
        {
            midPoints : double[]
            step : double
            range : double
        }

        member d.noOfIntervals = d.midPoints.Length

        member d.integrateValues (v : SparseArray) =
            let sum = v.value |> Array.map (fun e -> e.value) |> Array.sum
            sum * d.step

        static member create noOfIntervals minValue maxValue =
            let points = [| for i in 0..noOfIntervals -> minValue + (maxValue - minValue) * (double i) / (double noOfIntervals) |]

            {
                midPoints = [| for i in 0..noOfIntervals - 1 -> (points[i + 1] + points[i]) / 2.0 |]
                step =  (maxValue - minValue) / (double noOfIntervals)
                range = points[noOfIntervals] - points[0]
            }


    /// Data that describes a rectangle in ee * inf space.
    /// ee space is naturally limited to [-1, 1] unless we use a conformal transformation to extend it to [-Infinity, Infinity].
    /// inf (information) space is naturally limited at the lower bound (0). The upper bound can be rescaled to any number or even taken to Infinity.
    type Domain2D =
        {
            eeDomain : Domain
            infDomain : Domain
        }

        member d.integrateValues v =
            let sum = v |> Array.map (fun e -> e |> Array.sum) |> Array.sum
            sum * d.eeDomain.step * d.infDomain.step

        member d.integrateValues (a : SparseArray2D) =
            let sum = a.value |> Array.map (fun e -> e.value) |> Array.sum
            sum * d.eeDomain.step * d.infDomain.step

        member d.integrateValues (a : SparseArray2D, b : XY) =
            let bValue = b.value
            let sum = a.value |> Array.map (fun e -> e.value * bValue[e.i][e.j]) |> Array.sum
            sum * d.eeDomain.step * d.infDomain.step

        static member eeMinValue = -1.0
        static member eeMaxValue = 1.0
        static member infDefaultMinValue = 0.0

        static member create noOfIntervals l2 =
            {
                eeDomain = Domain.create noOfIntervals Domain2D.eeMinValue Domain2D.eeMaxValue
                infDomain = Domain.create noOfIntervals Domain2D.infDefaultMinValue l2
            }

    type MutationProbabilityData =
        {
            domain : Domain
            zeroThreshold : double
            epsFunc : double -> double
        }

        static member defaultZeroThreshold : double = 1.0e-05


    /// Creates a normalized mutation probability.
    /// The normalization is performed using integral estimate over the domain.
    type MutationProbability =
        | MutationProbability of SparseArray

        member r.value = let (MutationProbability v) = r in v

        /// m is a real (unscaled) value but e is scaled to the half of the range.
        static member create (data : MutationProbabilityData) mean =
            let epsFunc x = (data.epsFunc x) * data.domain.range / 2.0
            let f x = exp (- pown ((x - mean) / (epsFunc x)) 2)
            let values = data.domain.midPoints |> Array.map f |> SparseArray.create data.zeroThreshold
            let norm = data.domain.integrateValues values
            let p = values.value |> Array.map (fun v -> { v with value = v.value / norm }) |> SparseArray |> MutationProbability
            p


    type MutationProbabilityData2D =
        {
            eeMutationProbabilityData : MutationProbabilityData
            infMutationProbabilityData : MutationProbabilityData
        }

        member d.domain2D =
            {
                eeDomain = d.eeMutationProbabilityData.domain
                infDomain = d.infMutationProbabilityData.domain
            }


    type MutationProbability2D =
        | MutationProbability2D of SparseArray2D

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
        | MutationProbability4D of SparseArray2D[][]

        member r.value = let (MutationProbability4D v) = r in v

        static member create (data : MutationProbabilityData2D) =
            let eeMu = data.domain2D.eeDomain.midPoints
            let infMu = data.domain2D.infDomain.midPoints

            let values =
                eeMu
                |> Array.map(fun a -> infMu |> Array.map (MutationProbability2D.create data a))
                |> MutationProbability4D
            values

    type KernelData =
        {
            noOfIntervals : int
            l2 : double
            epsEe : double -> double
            epsInf : double -> double
        }


    type KernelValue =
        | KernelValue of SparseArray2D[][]

        member r.value = let (KernelValue v) = r in v


    /// Represents K(x, x1, y, y1) 2D Fredholm kernel.
    /// It is convenient to store it as [][] arrays in [][] array,
    /// where the first two indexes are (x, y) and last ones are (x1, y1).
    /// So the actual indexes here are K(x, y, x1, y1)
    type Kernel =
        {
            kernel : KernelValue
            domainData : Domain2D
        }

        member k.integrateValues (u : XY) =
            let v =
                k.kernel.value
                |> Array.map (fun e -> e |> Array.map (fun x -> k.domainData.integrateValues (x, u)))

            v

        static member defaultKernelFunc domainData data x1 y1 : XY =
            let v =
                domainData.eeDomain.midPoints
                |> Array.map (fun x -> domainData.infDomain.midPoints |> Array.map (fun y -> 1.0))
                |> XY

            v

        static member create data kernelFunc =
            let domainData = Domain2D.create data.noOfIntervals data.l2

            let kernel =
                domainData.eeDomain.midPoints
                |> Array.map (fun x -> domainData.infDomain.midPoints |> Array.map (kernelFunc domainData data x))
                |> KernelValue

            // let kernelData = kernelFunc data

            {
                kernel = kernel
                domainData = domainData
            }
