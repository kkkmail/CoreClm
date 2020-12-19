namespace Clm
open System

/// The distributions that we need fall into the following categories:
///     1. EE distributions. They must produce values on (-1, 1) and usually have mean of 0.
///     2. Unconditional rate distributions (RateDistribution). They must produce values on (0, infinity) with mean of 1.
//         This distribution is usually skewed toward 0.
///     3. Conditional rate distributions (RateDistribution).
///        They must produce values on (0, infinity) with mean of 1.
///        This distribution produces value near mean.
module Distributions =

    // https://en.wikipedia.org/wiki/Marsaglia_polar_method
    let rec getS (r : unit -> double) =
        let u = r() * 2.0 - 1.0
        let v = r() * 2.0 - 1.0
        let s = u * u + v * v

        if s > 0.0 && s < 1.0 then (s, u)
        else getS r


    let getGaussian (r : unit -> double) mean stdDev =
        let (s, u) = getS r
        let mul = -2.0 * (log s) / s |> sqrt
        mean + stdDev * u * mul


    let toSymmetricTriangular v =
        if v < 0.5 then -1.0 + sqrt(2.0 * v)
        else 1.0 - sqrt(2.0 * (1.0 - v))


    /// Generates only 0.
    let delta _ = 0.0

    /// Generates only -1 and 1 with equal probability.
    let biDelta v = if v < 0.5 then -1.0 else 1.0

    /// Generates values on (-1, 1).
    let uniform v = 2.0 * (v - 0.5)

    /// Generates values on (0, 3) with mean 1.
    let triangular v = 3.0 * (1.0 - sqrt(1.0 - v))

     /// Generates values on (-1, 1) with max / mean at 0.
    let symmetricTriangular v = v |> toSymmetricTriangular


    type ReactionRate =
        | ReactionRate of double


    [<Literal>]
    let DistributionTypeName = "DistributionType"

    [<Literal>]
    let DeltaName = "Delta"

    [<Literal>]
    let BiDeltaName = "BiDelta"

    [<Literal>]
    let UniformName = "Uniform"

    [<Literal>]
    let TriangularName = "Triangular"

    [<Literal>]
    let SymmetricTriangularName = "SymmetricTriangular"


    type DistributionType =
        | Delta
        | BiDelta
        | Uniform
        | Triangular
        | SymmetricTriangular

        member d.transform =
            match d with
            | Delta -> delta
            | BiDelta -> biDelta
            | Uniform -> uniform
            | Triangular -> triangular
            | SymmetricTriangular -> symmetricTriangular

        member d.mean =
            match d with
            | Delta -> 0.0
            | BiDelta -> 0.0
            | Uniform -> 0.0
            | Triangular -> 1.0
            | SymmetricTriangular -> 0.0

        member d.stdDev =
            match d with
            | Delta -> 0.0
            | BiDelta -> 1.0 / 2.0 |> sqrt
            | Uniform -> 1.0 / 3.0 |> sqrt
            | Triangular -> 1.0 / 2.0 |> sqrt
            | SymmetricTriangular -> 1.0 / 6.0 |> sqrt


    type DistributionParams =
        {
            threshold : double option
            scale : double option
            shift : double option
        }

        static member defaultValue =
            {
                threshold = None
                scale = None
                shift = None
            }

        static member create threshold scale shift =
            {
                threshold = threshold
                scale = scale
                shift = shift
            }


    type DistributionParamsWithType =
        {
            distributionType : DistributionType
            distributionParams : DistributionParams
        }


    type RandomValueGetter =
        {
            seed : int
            next : unit -> int
            nextN : int -> int
            nextDouble : unit -> double
        }

        static member create so =
            let seed =
                match so with
                | Some s -> s
                | None -> Random().Next()

            let rnd = Random(seed)

            {
                seed = seed
                next = rnd.Next
                nextN = (fun n -> rnd.Next(n))
                nextDouble = rnd.NextDouble
            }

        static member create() = RandomValueGetter.create None


    type SuccessNumberType =
        | RandomValueBased
        | ThresholdBased


    /// Generates number of successes using either:
    ///    1. Given distribution (RandomValueGetter).
    ///    2. Value of threshold parameter of distribution.
    type SuccessNumberGetter =
        | RandomValueGetterBased of RandomValueGetter
        | ThresholdValueBased of RandomValueGetter

        member this.randomValueGetter =
            match this with | RandomValueGetterBased r | ThresholdValueBased r -> r


    /// First scale, then shift. This is more convenient here than the other way around.
    type Distribution =
        | Distribution of DistributionParamsWithType

        member this.value = let (Distribution v) = this in v

        member d.isDefined (rnd : RandomValueGetter) =
            match d.value.distributionParams.threshold with
            | Some t -> rnd.nextDouble() < t
            | None -> true

        member private d.scale x =
            x *
            match d.value.distributionParams.scale with
            | Some s -> s
            | None -> 1.0

        member private d.shift x =
            x +
            match d.value.distributionParams.shift with
            | Some s -> s
            | None -> 0.0

        member private d.scaleShift x = x |> d.scale |> d.shift

        member d.distributionParams = d.value.distributionParams
        member d.nextDouble (rnd : RandomValueGetter) = rnd.nextDouble() |> d.value.distributionType.transform |> d.scaleShift

        member d.mean = d.value.distributionType.mean |> d.scaleShift
        member d.stdDev = d.value.distributionType.stdDev |> d.scale

        member d.nextDoubleOpt rnd =
            match d.isDefined rnd with
            | true -> d.nextDouble rnd |> Some
            | false -> None

        member d.createScaled newScale = { d.value with distributionParams = { d.value.distributionParams with scale = newScale } } |> Distribution
        member d.createShifted newShift = { d.value with distributionParams = { d.distributionParams with shift = newShift } } |> Distribution
        member d.createThresholded newThreshold = { d.value with distributionParams = { d.distributionParams with threshold = newThreshold } } |> Distribution
        member __.next (rnd : RandomValueGetter) = rnd.next()
        member __.nextN (rnd : RandomValueGetter) n = rnd.nextN n

        member d.successNumber (s : SuccessNumberGetter) (noOfTries : int64) =
            match d.value.distributionParams.threshold with
            | Some p ->
                let mean = 1.0
                let m = (mean * p) * (double noOfTries)

                let sn =
                    match s with
                        | RandomValueGetterBased rnd ->
                            let stdDev = 0.0
                            let s = (stdDev * stdDev + p * (1.0 - p) * mean * mean) * (double noOfTries) |> sqrt
                            getGaussian rnd.nextDouble m s
                        | ThresholdValueBased _ -> double m
                printfn "successNumber: noOfTries = %A, p = %A, m = %A, s = %A, sn = %A" noOfTries p m s sn
                min (max 0L (int64 sn)) noOfTries |> int
            | None -> noOfTries |> int

        static member createDelta p = { distributionType = Delta; distributionParams = p } |> Distribution
        static member createBiDelta p = { distributionType = BiDelta; distributionParams = p } |> Distribution
        static member createUniform p = { distributionType = Uniform; distributionParams = p } |> Distribution
        static member createTriangular p = { distributionType = Triangular; distributionParams = p } |> Distribution
        static member createSymmetricTriangular p = { distributionType = SymmetricTriangular; distributionParams = p } |> Distribution


    /// EE distributions. They are specially formatted distributions to return values only between [-1 and 1].
    type EeDistribution =
        | EeDistribution of Distribution

        member eed.nextDouble rnd =
            let (EeDistribution d) = eed
            max (min (d.nextDouble rnd) 1.0) (-1.0)

        static member createSymmetricTriangular() =
            Distribution.createSymmetricTriangular { threshold = None; scale = None; shift = None } |> EeDistribution

        static member createBiDelta scale =
            Distribution.createBiDelta { threshold = None; scale = scale; shift = None } |> EeDistribution

        static member private getMeanAndWidth mean =
            match mean with
            | x when x <= -1.0 -> -1.0, None
            | x when -1.0 < x && x < 1.0 -> x, min (1.0 - x) (x + 1.0) |> Some
            | x when x >= 1.0 -> 1.0, None
            | _ -> 0.0, Some 1.0

        static member private createCenteredDelta mean =
            let m, _ = EeDistribution.getMeanAndWidth mean
            Distribution.createDelta { threshold = None; scale = None; shift = Some m } |> EeDistribution

        static member private createCentered mean =
            let m, w = EeDistribution.getMeanAndWidth mean

            match w with
            | Some s -> Distribution.createSymmetricTriangular { threshold = None; scale = Some s; shift = Some m } |> EeDistribution
            | None -> EeDistribution.createCenteredDelta mean

        static member getDeltaEeDistrOpt (rate : ReactionRate option) (rateEnant : ReactionRate option) =
            match rate, rateEnant with
            | Some (ReactionRate r), Some (ReactionRate re) -> (r - re) / (r + re) |> EeDistribution.createCenteredDelta |> Some
            | _ -> None

        static member getCenteredEeDistrOpt (rate : ReactionRate option) (rateEnant : ReactionRate option) =
            match rate, rateEnant with
            | Some (ReactionRate r), Some (ReactionRate re) -> (r - re) / (r + re) |> EeDistribution.createCentered |> Some
            | _ -> None

        static member getDefaultEeDistrOpt = EeDistribution.getCenteredEeDistrOpt


    type EeDistributionGetter =
        | NoneEeGetter
        | DeltaEeDistributionGetter
        | CenteredEeDistributionGetter

        member ee.getDistr =
            match ee with
            | NoneEeGetter -> (fun _ _ -> None)
            | DeltaEeDistributionGetter -> EeDistribution.getDeltaEeDistrOpt
            | CenteredEeDistributionGetter -> EeDistribution.getCenteredEeDistrOpt


    /// Distribution of rate multipliers for catalytic reactions.
    type RateMultiplierDistribution =
        | NoneRateMult
        | RateMultDistr of Distribution

        static member private normalize d = d |> Option.bind (fun e -> max e 0.0 |> Some)

        member this.value =
            match this with
            | NoneRateMult -> None
            | RateMultDistr d -> Some d

        member this.nextDoubleOpt rnd =
            match this with
            | NoneRateMult -> None
            | RateMultDistr d -> d.nextDoubleOpt rnd |> RateMultiplierDistribution.normalize

        member this.nextDouble rnd =
            match this with
            | NoneRateMult -> None
            | RateMultDistr d -> d.nextDouble rnd |> Some |> RateMultiplierDistribution.normalize

        static member createNone = NoneRateMult

        static member createDelta threshold rate =
            Distribution.createDelta { threshold = threshold; scale = None; shift = Some rate } |> RateMultDistr

        static member createTriangular threshold rate =
            Distribution.createTriangular { threshold = threshold; scale = Some rate; shift = None } |> RateMultDistr

        static member createSymmetricTriangular threshold rate =
            Distribution.createSymmetricTriangular { threshold = threshold; scale = Some rate; shift = Some rate } |> RateMultDistr


    type RateMultiplierDistributionGetter =
        | NoneRateMultDistrGetter
        | DeltaRateMultDistrGetter
        | TriangularRateMultDistrGetter
        | SymmetricTriangularRateMultDistrGetter

        member this.getDistr threshold rate =
            match this with
            | NoneRateMultDistrGetter -> RateMultiplierDistribution.createNone
            | DeltaRateMultDistrGetter -> RateMultiplierDistribution.createDelta threshold rate
            | TriangularRateMultDistrGetter -> RateMultiplierDistribution.createTriangular threshold rate
            | SymmetricTriangularRateMultDistrGetter -> RateMultiplierDistribution.createSymmetricTriangular threshold rate
