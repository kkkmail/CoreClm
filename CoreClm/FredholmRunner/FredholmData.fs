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
            j : int
            value : double
        }


    // /// Represents indexes of 2D sparse array.
    // type IJ = int[][]
    //
    // type Sparse =
    //     {
    //         a : int[][]
    //     }


    /// Describes a function domain suitable for integral approximation.
    /// Equidistant grid is used to reduce the number of multiplications.
    type Domain =
        {
            midPoints : double[]
            step : double
        }

        // member d.minValue = d.points[0]
        // member d.maxValue = d.points[d.points.Length - 1]
        // member d.range = (d.minValue, d.maxValue)
        // member d.noOfPoints = d.points.Length
        member d.noOfIntervals = d.midPoints.Length

        member d.integrateValues v =
            let sum = v |> Array.sum
            sum * d.step


        // member d.integrate f =
        //     let values = d.midPoints |> Array.map f
        //     d.integrateValues values

        static member create noOfIntervals minValue maxValue =
            let points = [| for i in 0..noOfIntervals -> minValue + (maxValue - minValue) * (double i) / (double noOfIntervals) |]

            {
                midPoints = [| for i in 0..noOfIntervals - 1 -> (points[i + 1] + points[i]) / 2.0 |]
                step =  (maxValue - minValue) / (double noOfIntervals)
            }


    /// Data that describes a rectangle in ee * information space.
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

        member d.integrateValues (a : XY, b : XY) =
            let sum = b |> Array.mapi (fun i e -> e |> Array.mapi (fun j y -> a[i][j] * y)|> Array.sum) |> Array.sum
            sum * d.eeDomain.step * d.infDomain.step

        static member eeMinValue = -1.0
        static member eeMaxValue = 1.0
        static member infDefaultMinValue = 0.0

        static member create noOfIntervals l2 =
            {
                eeDomain = Domain.create noOfIntervals DomainData.eeMinValue DomainData.eeMinValue
                infDomain = Domain.create noOfIntervals DomainData.infDefaultMinValue l2
            }


    /// Creates a normalized probability
    type MutationProbability =
        {
            p : double[]
        }

        static member create (d : Domain) m e =
            let f x = exp (- pown ((x - m) / e) 2)
            let values = d.midPoints |> Array.map f
            let norm = d.integrateValues values
            let p = values |> Array.map (fun v -> v / norm)

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


    type KernelValue = XY[][]


    /// Represents K(x, x1, y, y1) 2D Fredholm kernel.
    /// It is convenient to store it as [][] arrays in [][] array,
    /// where the first two indexes are (x, y) and last ones are (x1, y1).
    /// So the actual indexes here are K(x, y, x1, y1)
    type Kernel =
        {
            kernel : KernelValue
            domainData : DomainData
        }

        member k.integrateValues u =
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
