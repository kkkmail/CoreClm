namespace FredholmSolver

open Softellect.Sys.Primitives
open FredholmSolver.Primitives
open FredholmSolver.Kernel
open Softellect.Sys.Core
open Softellect.Math.Primitives
open Softellect.Math.Models

module Common =

    type SolutionMethod =
        | Euler    // https://en.wikipedia.org/wiki/Euler_method
        | MidPoint // https://en.wikipedia.org/wiki/Midpoint_method - Not implemented yet.


    /// All parameterization of the solution method.
    type SolutionMethodParams =
        {
            solutionMethod : SolutionMethod
            useParallel : bool
        }

        static member defaultValue =
            {
                solutionMethod = Euler
                useParallel = false
            }

        member s.modelString =
            let sm =
                match s.solutionMethod with
                | Euler -> EmptyString
                | MidPoint -> "_mp"

            let p = if s.useParallel then "_p" else EmptyString
            sm + p


    /// We can only shift delta to a grid cell.
    type ProtocellInitParams =
        | DeltaEeInfShifted of (int * int)

        static member create shift = DeltaEeInfShifted (shift, 0)
        static member defaultValue = ProtocellInitParams.create 0

        member p.modelString =
            match p with
            // | DeltaEeShifted eeShift -> if eeShift = 0 then None else Some $"s{eeShift}"
            | DeltaEeInfShifted (eeShift, infShift) ->
                match eeShift, infShift with
                | 0, 0 -> None
                | _, 0 -> Some $"s{eeShift}"
                | 0, _ -> Some $"s#{infShift}"
                | _ -> Some $"s{eeShift}#{infShift}"

        /// Creates a "delta" function centered near (0, 0) in the domain,
        /// which is a middle point in ee domain and 0-th point in inf domain.
        member private p.calculateU eps (getNorm : Matrix<double> -> double) (domain : Domain2D) =
            match p with
            // | DeltaEeShifted eeShift ->
            //     let domainIntervals = domain.eeDomain.noOfIntervals
            //     let g i j =
            //         match domainIntervals % 2 = 0 with
            //         | true -> if ((i + eeShift) * 2 = domainIntervals) && (j = 0) then 1.0 else 0.0
            //         | false ->
            //             if (((i + eeShift) * 2 = domainIntervals - 1) || ((i + eeShift) * 2 = domainIntervals + 1)) && (j = 0) then 1.0 else 0.0
            //
            //     let v = domain.eeDomain.points.value |> Array.mapi (fun i _ -> domain.infDomain.points.value |> Array.mapi (fun j _ -> g i j)) |> Matrix
            //     let norm = getNorm v
            //     (eps / norm) * v
            | DeltaEeInfShifted (eeShift, infShift) ->
                let domainIntervals = domain.eeDomain.noOfIntervals.value
                let g i j =
                    match domainIntervals % 2 = 0 with
                    | true -> if ((i + eeShift) * 2 = domainIntervals) && (j = infShift) then 1.0 else 0.0
                    | false -> if (((i + eeShift) * 2 = domainIntervals - 1) || ((i + eeShift) * 2 = domainIntervals + 1)) && (j = infShift) then 1.0 else 0.0

                let v = domain.eeDomain.points.value |> Array.mapi (fun i _ -> domain.infDomain.points.value |> Array.mapi (fun j _ -> g i j)) |> Matrix
                let norm = getNorm v
                (eps / norm) * v

        /// Creates a "delta" function centered near the middle of the domain.
        member private p.calculateSymmetricU eps (getNorm : Matrix<double> -> double) (domain : Domain2D) =
            match p with
            | DeltaEeInfShifted (eeShift, infShift) ->
                let domainIntervals = domain.eeDomain.noOfIntervals.value
                let g i j =
                    match domainIntervals % 2 = 0 with
                    | true -> if ((i + eeShift) * 2 = domainIntervals) && ((j + infShift) * 2 = domainIntervals) then 1.0 else 0.0
                    | false -> if (((i + eeShift) * 2 = domainIntervals - 1) || ((i + eeShift) * 2 = domainIntervals + 1)) && (((j + infShift) * 2 = domainIntervals - 1) || ((j + infShift) * 2 = domainIntervals + 1)) then 1.0 else 0.0

                let v = domain.eeDomain.points.value |> Array.mapi (fun i _ -> domain.infDomain.points.value |> Array.mapi (fun j _ -> g i j)) |> Matrix
                let norm = getNorm v
                (eps / norm) * v

        member p.getU eps (domain : Domain2D) = p.calculateU eps domain.integrateValues domain
        member p.getSymmetricU eps (domain : Domain2D) = p.calculateSymmetricU eps domain.integrateValues domain

        member p.getIntU (uTotal : int64) (domain : Domain2D) =
            let m = p.calculateU (double uTotal) _.total() domain
            m.convert int64

        member p.getSymmetricIntU (uTotal : int64) (domain : Domain2D) =
            let m = p.calculateSymmetricU (double uTotal) _.total() domain
            m.convert int64


    type EeInfIntInitParams =
        {
            uInitial : MoleculeCount
            protocellInitParams : ProtocellInitParams
            totalMolecules : MoleculeCount
            seedValue : int
        }

        static member defaultValue =
            {
                uInitial = MoleculeCount.OneThousand
                protocellInitParams = ProtocellInitParams.defaultValue
                totalMolecules = MoleculeCount.OneBillion
                seedValue = 1
            }

        member p.modelString =
            let df = EeInfIntInitParams.defaultValue
            let f =  toModelStringInt64 df.totalMolecules.value p.totalMolecules.value |> bindPrefix "f"
            let u =  toModelStringInt64 df.uInitial.value p.uInitial.value |> bindPrefix "u"
            let s = p.protocellInitParams.modelString
            let r =  if df.seedValue = p.seedValue then None else Some p.seedValue |> bindPrefix "f"
            [| f; u; s; r |] |> Array.choose id |> joinStrings EmptyString

        member p.shifted shift = { p with protocellInitParams = ProtocellInitParams.create shift }
        member p.withTotalMolecules totalMolecules = { p with totalMolecules = totalMolecules }
        member p.withUInitial uInitial = { p with uInitial = uInitial }


    type EeInfIntModelParams =
        {
            eeInfModelParams : EeInfModelParams
            intInitParams : EeInfIntInitParams
            solutionMethodParams : SolutionMethodParams
        }

        member _.evolutionType = EvolutionType.DiscreteEvolution
        member p.shifted shift = { p with intInitParams = p.intInitParams.shifted shift }
        member p.named n = { p with eeInfModelParams.name = n }
        member p.withTotalMolecules totalMolecules = { p with intInitParams = p.intInitParams.withTotalMolecules totalMolecules }
        member p.withUInitial uInitial = { p with intInitParams = p.intInitParams.withUInitial uInitial }
        member p.withInfMaxValue (infMaxValue : InfMaxValue) = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withInfMaxValue infMaxValue }
        member p.withProtocellInitParams protocellInitParams = { p with intInitParams = { p.intInitParams with protocellInitParams = protocellInitParams } }
        member p.modelString = $"{p.eeInfModelParams.modelString}{p.solutionMethodParams.modelString}{p.intInitParams.modelString}"

        /// Default linear value, mostly for tests, as it does not have many practical usages.
        static member defaultValue =
            {
                eeInfModelParams = EeInfModelParams.defaultValue
                intInitParams = EeInfIntInitParams.defaultValue
                solutionMethodParams = SolutionMethodParams.defaultValue
            }

        static member defaultSymmetricValue =
            {
                eeInfModelParams = EeInfModelParams.defaultSymmetricValue
                intInitParams = EeInfIntInitParams.defaultValue
                solutionMethodParams = SolutionMethodParams.defaultValue
            }

        /// Default value with quadratic kernel and non-linear gamma.
        /// This is the main starting point where we can vary k0, eps0, gamma0, etc...
        static member defaultNonLinearValue =
            { EeInfIntModelParams.defaultValue with eeInfModelParams = EeInfModelParams.defaultNonLinearValue }

        /// Default SYMMETRIC value with quadratic kernel and non-linear gamma.
        /// This is the main starting point where we can vary k0, eps0, gamma0, etc...
        static member defaultSymmetricNonLinearValue =
            { EeInfIntModelParams.defaultSymmetricValue with eeInfModelParams = EeInfModelParams.defaultSymmetricNonLinearValue }

        /// Default value with quadratic kernel and non-linear gamma.
        /// This is the main starting point where we can vary k0, eps0, gamma0, etc...
        static member defaultQuadraticWithLinearInfValue =
            { EeInfIntModelParams.defaultValue with eeInfModelParams = EeInfModelParams.defaultQuadraticWithLinearInfValue }

        static member withK0 k0 p = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withK0 k0 }
        static member withKaFunc (ka : KaFuncValue) p = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withKaFunc ka }
        static member withEps0 eps0 p = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withEps0 eps0 }
        static member withGamma0 gamma0 p = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withGamma0 gamma0 }
        static member withDomainIntervals d p = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withDomainIntervals d }
        static member withGlobalAsymmetryFactor a p = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withGlobalAsymmetryFactor a }


    type FileSuffix
        with
        static member EeSuffix = FileSuffix "ee"
        static member InfSuffix = FileSuffix "inf"
        static member GammaSuffix = FileSuffix "gamma"
        static member KaSuffix = FileSuffix "ka"
