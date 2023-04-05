namespace FredholmRunner

module FredholmData =

    /// TODO kk:20230402 - Exists in Gillespie.SsaPrimitives - Consolidate.
    type EnantiomericExcess =
        | EnantiomericExcess of double

        member r.value = let (EnantiomericExcess v) = r in v


    /// Describes a function domain suitable for integral approximation.
    type Domain =
        {
            points : double[]
            midPoints : double[]
            intervals : double[]
        }

        member d.minValue = d.points[0]
        member d.maxValue = d.points[d.points.Length - 1]
        member d.range = (d.minValue, d.maxValue)
        member d.noOfPoints = d.points.Length
        member d.noOfIntervals = d.intervals.Length

        member d.integrateValues v =
            d.intervals
            |> Array.zip v
            |> Array.map (fun (a, b) -> a * b)
            |> Array.sum

        member d.integrate f =
            let values = d.midPoints |> Array.map f
            d.integrateValues values

        static member create noOfIntervals minValue maxValue =
            let points = [| for i in 0..noOfIntervals -> minValue + (maxValue - minValue) * (double i) / (double noOfIntervals) |]

            {
                points = points
                midPoints = [| for i in 0..noOfIntervals - 1 -> (points[i + 1] + points[i]) / 2.0 |]
                intervals = [| for i in 0..noOfIntervals - 1 -> (points[i + 1] - points[i]) |]
            }


    /// Data that describes a rectangle in ee * information space.
    /// ee space is naturally limited to [-1, 1] unless we use a conformal transformation to extend it to [-Infinity, Infinity].
    /// inf (information) space is naturally limited at the lower bound (0). The upper bound can be rescaled to any number or even taken to Infinity.
    type DomainData =
        {
            eeDomain : Domain
            infDomain : Domain
        }

        static member eeMinValue = -1.0
        static member eeMaxValue = 1.0
        static member infDefaultMinValue = 0.0

        static member create n l2 =
            let noOfIntervals = 2 * n

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
            let norm = d.integrate f
            let p = d.midPoints |> Array.map (fun x -> (f x) / norm)

            {
                p = p
            }
