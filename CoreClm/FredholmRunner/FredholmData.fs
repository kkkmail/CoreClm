namespace FredholmRunner

module FredholmData =

    /// TODO kk:20230402 - Exists in Gillespie.SsaPrimitives - Consolidate.
    type EnantiomericExcess =
        | EnantiomericExcess of double

        member r.value = let (EnantiomericExcess v) = r in v


    /// Data that describe a rectangle in ee * information space.
    /// ee space is naturally limited to [-1, 1] unless we use a conformal transformation to extend it to [-Infinity, Infinity].
    /// inf (information) space is naturally limited at the lower bound (0). The upper bound can be rescaled to any number or even taken to Infinity.
    type DomainData =
        {
            eeNumberOfPoints : int
            eePoints : double[]
            infNumberOfPoints : int
            infMinValue : double
            infMaxValue : double
            infPoints : double[]
        }

        static member eeMinValue = -1.0
        static member eeMaxValue = 1.0
        static member infDefaultMinValue = 0.0
        member e.eeRange = (DomainData.eeMinValue, DomainData.eeMaxValue)
        member e.infRange = (e.infMinValue, e.infMaxValue)

        static member create n l2 =
            let noOfPoints = 2 * n + 1
            let infMinValue = DomainData.infDefaultMinValue
            let eePoints = [| for i in 0..noOfPoints -> DomainData.eeMinValue + (DomainData.eeMaxValue - DomainData.eeMinValue) * (double i) / (double noOfPoints) |]
            let infPoints = [| for i in 0..noOfPoints -> infMinValue + (l2 - infMinValue) * (double i) / (double noOfPoints) |]

            {
                eeNumberOfPoints = noOfPoints
                eePoints = eePoints
                infNumberOfPoints = noOfPoints
                infMinValue = infMinValue
                infMaxValue = l2
                infPoints = infPoints
            }


    type MutationProbability =
        {
            a : int
        }
