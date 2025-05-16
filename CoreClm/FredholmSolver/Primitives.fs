namespace FredholmSolver

open FSharp.Collections
open MathNet.Numerics.Distributions
open System
open Primitives.VersionInfo
open Softellect.Sys.Primitives
open Softellect.Sys.Core

module Primitives =

    [<Literal>]
    let DefaultRootFolder = DefaultRootDrive + @":\" + ContGenBaseName + @"\Clm\"

    [<Literal>]
    let DefaultResultLocationFolder = DefaultRootFolder + "Results"

    [<Literal>]
    let DefaultFileStorageFolder = DefaultRootFolder + "FileStorage"

    let bindPrefix p v = v |> Option.bind (fun e -> Some $"{p}{e}")


    let toDoubleString (actualValue: double) : string =
        let actualValDec = decimal actualValue
        let roundedActualVal = Decimal.Round(actualValDec, 10)

        // Local function to extract decimal part from a decimal number.
        let extractDecimalPart (decValue: decimal) =
            if decValue = 0.0M then "0"
            else
                let str = string decValue
                str.Split('.').[1]

        if roundedActualVal < 1.0M then extractDecimalPart roundedActualVal
        else
            // If actualValue is greater than or equal to 1.
            let wholePart = int roundedActualVal
            let decimalPartStr = extractDecimalPart (roundedActualVal - decimal wholePart)
            $"%d{wholePart}_%s{decimalPartStr}"


    /// Return an optional string to be used in model name generation.
    let toModelString (defaultValue: double) (actualValue: double) : string option =
        let epsilon = 1e-10M // tolerance for comparing decimals

        // Convert doubles to decimals and round to eliminate noise.
        let defaultValDec = decimal defaultValue
        let actualValDec = decimal actualValue
        let roundedActualVal = Decimal.Round(actualValDec, 10)

        // Check if the values are effectively the same.
        if Math.Abs (defaultValDec - roundedActualVal) < epsilon then None
        else toDoubleString actualValue |> Some


    let private powers = [ ("K", 1_000L); ("M", 1_000_000L); ("G", 1_000_000_000L); ("T", 1_000_000_000_000L); ("P", 1_000_000_000_000_000L); ("E", 1_000_000_000_000_000_000L) ]


    /// Return an optional string to be used in model name generation.
    let toModelStringInt64 (defaultValue: int64) (actualValue: int64) : string option =
        if defaultValue = actualValue then None
        else
            let suffix, power = powers |> List.find (fun (_, power) -> (actualValue / power) / 1000L = 0L)
            let adjustedValue = double actualValue / double power
            let formattedString = toDoubleString adjustedValue

            if formattedString.EndsWith("_0")
            then formattedString.Replace("_0", suffix)
            else formattedString.Replace("_", suffix)
            |> Some


    /// Return an optional string to be used in model name generation.
    let toModelStringArray (defaultValues: double array) (actualValues: double array) : string option =
        let separator = "#"
        let pairOptionToStr opt = opt |> Option.defaultValue EmptyString
        let toCommonStr a b = Array.map2 toModelString a b |> Array.map pairOptionToStr |> joinStrings "#"

        if defaultValues.Length = actualValues.Length then
            let pairedOptions = Array.map2 toModelString defaultValues actualValues

            match pairedOptions |> Array.tryFindIndex (fun x -> x.IsSome) with
            | Some i ->
                match i with
                | 1 -> pairedOptions[1]
                | _ -> Array.map pairOptionToStr pairedOptions |> joinStrings separator |> Some
            | None -> None
        else if defaultValues.Length > actualValues.Length then
            let defaultValuesShort = defaultValues |> Array.take actualValues.Length
            let commonStr = toCommonStr defaultValuesShort actualValues
            let missingDefaults = defaultValues[actualValues.Length..] |> Array.map toDoubleString |> joinStrings "#"
            Some $"!{commonStr}{separator}{missingDefaults}"
        else
            let actualValuesShort = actualValues |> Array.take defaultValues.Length
            let commonStr = toCommonStr defaultValues actualValuesShort
            let extraActuals = actualValues[defaultValues.Length..] |> Array.map toDoubleString |> joinStrings "#"
            Some $"{commonStr}{separator}{extraActuals}"


    let poissonSample rnd lambda =
        if lambda <= 2e9 then
            // Use MathNet.Numerics.Distributions for small lambda
            try
                int64 (Poisson.Sample(rnd, lambda))
            with e ->
                failwith $"lambda: {lambda}, exception: {e}"
        else
            // Use Gaussian approximation for large lambda
            let mu = lambda
            let sigma = sqrt lambda
            let sample = Normal.Sample(rnd, mu, sigma)
            int64 (Math.Round(sample))


    /// Encapsulation of a Poisson distribution sampler.
    /// It takes a value of lambda and returns next random number of events.
    type PoissonSingleSampler =
        | PoissonSingleSampler of (float -> int64)

        member inline private r.value = let (PoissonSingleSampler v) = r in v
        member r.nextPoisson lambda = r.value lambda
        static member create rnd = poissonSample rnd |> PoissonSingleSampler


    /// Encapsulation of a Poisson distribution sampler factory suitable for both sequential and parallel code.
    type PoissonMultiSampler =
        {
            sampler : PoissonSingleSampler
            parallelSampler : PoissonSingleSampler[]
        }

        static member create n (rnd : Random) =
            let r() = Random(rnd.Next())
            let sampler = PoissonSingleSampler.create (r())
            let parallelSampler = [| for _ in 0..(n - 1) -> PoissonSingleSampler.create (r()) |]
            {
                sampler = sampler
                parallelSampler = parallelSampler
            }


    type PoissonSampler =
        | SingleSampler of PoissonSingleSampler
        | MultiSampler of PoissonMultiSampler

        member p.sampler =
            match p with
            | SingleSampler s -> s
            | MultiSampler s -> s.sampler

        member p.getSampler i =
            match p with
            | SingleSampler s -> s
            | MultiSampler s -> s.parallelSampler[i]

        member p.length =
            match p with
            | SingleSampler _ -> 0
            | MultiSampler s -> s.parallelSampler.Length

        static member createMultiSampler n rnd = PoissonMultiSampler.create n rnd |> MultiSampler
        static member createSingleSampler rnd = PoissonSingleSampler.create rnd |> SingleSampler


    type EvolutionType =
        | DifferentialEvolution
        | DiscreteEvolution


    /// A parameter to control when a value in a sparse array should be treated as exact zero (and ignored).
    type ZeroThreshold =
        | ZeroThreshold of double

        member r.value = let (ZeroThreshold v) = r in v
        static member defaultValue = ZeroThreshold 1.0e-05
