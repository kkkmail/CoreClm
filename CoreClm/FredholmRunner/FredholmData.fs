namespace FredholmRunner

open FSharp.Collections

module FredholmData =

    /// TODO kk:20230402 - Exists in Gillespie.SsaPrimitives - Consolidate.
    type EnantiomericExcess =
        | EnantiomericExcess of double

        member r.value = let (EnantiomericExcess v) = r in v


    /// Representation of 2D values.
    /// It is convenient to store them as [][] rather than as [,] to leverage build in F# array manipulations.
    type XY = double[][]


    type SparseValue =
        {
            i : int
            value : double
        }


    type SparseArray = SparseValue[]


    type SparseValue
        with
        static member createArray z (v : double[]) : SparseArray =
            v
            |> Array.mapi (fun i e -> if e >= z then Some { i = i; value = e } else None)
            |> Array.choose id


    type SparseValue2D =
        {
            i : int
            j : int
            value : double
        }


    type SparseArray2D = SparseValue2D[]


    type SparseValue2D
        with
        static member createArray2D z (v : double[][]) : SparseArray2D =
            v
            |> Array.mapi (fun i e -> e |> Array.mapi (fun j v -> if v >= z then Some { i = i; j = j; value = v } else None))
            |> Array.concat
            |> Array.choose id

        static member cartesianMultiply (a : SparseArray) (b : SparseArray) : SparseArray2D =
            a |> Array.map (fun e -> b |> Array.map (fun f -> { i = e.i; j = f.i; value = e.value * f.value })) |> Array.concat


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
            let sum = v |> Array.map (fun e -> e.value) |> Array.sum
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
    type DomainData =
        {
            eeDomain : Domain
            infDomain : Domain
        }

        member d.integrateValues v =
            let sum = v |> Array.map (fun e -> e |> Array.sum) |> Array.sum
            sum * d.eeDomain.step * d.infDomain.step

        member d.integrateValues (a : SparseArray2D) =
            let sum = a |> Array.map (fun e -> e.value) |> Array.sum
            sum * d.eeDomain.step * d.infDomain.step

        member d.integrateValues (a : SparseArray2D, b : XY) =
            let sum = a |> Array.map (fun e -> e.value * b[e.i][e.j]) |> Array.sum
            sum * d.eeDomain.step * d.infDomain.step

        static member eeMinValue = -1.0
        static member eeMaxValue = 1.0
        static member infDefaultMinValue = 0.0

        static member create noOfIntervals l2 =
            {
                eeDomain = Domain.create noOfIntervals DomainData.eeMinValue DomainData.eeMaxValue
                infDomain = Domain.create noOfIntervals DomainData.infDefaultMinValue l2
            }


    /// Creates a normalized probability.
    type MutationProbability =
        {
            p : SparseArray
        }

        static member defaultZeroValue : double = 1.0e-05

        /// m is a real (unscaled) value but e is scaled to the half of the range.
        static member create (d : Domain) z m e =
            let eValue = e * d.range / 2.0
            let f x = exp (- pown ((x - m) / eValue) 2)
            let values = d.midPoints |> Array.map f |> SparseValue.createArray z
            let norm = d.integrateValues values
            let p = values |> Array.map (fun v -> { v with value = v.value / norm })

            {
                p = p
            }


    type KernelData =
        {
            noOfIntervals : int
            l2 : double
            epsEe : double
            epsInf : double
        }


    // type KernelValue = XY[][]
    type KernelValue = SparseArray2D[][]


    /// Represents K(x, x1, y, y1) 2D Fredholm kernel.
    /// It is convenient to store it as [][] arrays in [][] array,
    /// where the first two indexes are (x, y) and last ones are (x1, y1).
    /// So the actual indexes here are K(x, y, x1, y1)
    type Kernel =
        {
            kernel : KernelValue
            domainData : DomainData
        }

        member k.integrateValues u : XY =
            let v =
                k.kernel
                |> Array.map (fun e -> e |> Array.map (fun x -> k.domainData.integrateValues (x, u)))

            v

        static member defaultKernelFunc domainData data x1 y1 : XY =
            let v =
                domainData.eeDomain.midPoints
                |> Array.map (fun x -> domainData.infDomain.midPoints |> Array.map (fun y -> 1.0))

            v

        static member create data kernelFunc =
            let domainData = DomainData.create data.noOfIntervals data.l2

            let kernelData =
                domainData.eeDomain.midPoints
                |> Array.map (fun x -> domainData.infDomain.midPoints |> Array.map (kernelFunc domainData data x))

            // let kernelData = kernelFunc data

            {
                kernel = kernelData
                domainData = domainData
            }
