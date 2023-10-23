namespace FredholmSolver

open FredholmSolver.Primitives
open FredholmSolver.Kernel
open GenericOdeSolver.Primitives

module EeInfDiffModel =

    type SubstanceType =
        | Food
        | Waste
        | Protocell


    type FoodData =
        | FoodData of double

        member r.value = let (FoodData v) = r in v


    type WasteData =
        | WasteData of double

        member r.value = let (WasteData v) = r in v


    type ProtocellData =
        | ProtocellData of Matrix<double>

        member r.value = let (ProtocellData v) = r in v


    type SubstanceData =
        | SubstanceData of LinearData<SubstanceType, double>

        member r.value = let (SubstanceData v) = r in v

        static member create (x : FoodData) (w: WasteData) (p : ProtocellData) =
            let retVal =
                LinearData<SubstanceType, double>.defaultValue
                    .append(Food, x.value)
                    .append(Waste, w.value)
                    .append(Protocell, p.value)
                |> SubstanceData

            retVal

        member d.food =
            let v = d.value[Food]

            match v.dataType with
            | ScalarData -> d.value.data[v.start]
            | _ -> failwith $"Incorrect data type: {v.dataType} for scalar data Food."

        member d.waste =
            let v = d.value[Waste]

            match v.dataType with
            | ScalarData -> d.value.data[v.start]
            | _ -> failwith $"Incorrect data type: {v.dataType} for scalar data Waste."

        member d.protocell =
            let v = d.value[Protocell]

            match v.dataType with
            | MatrixData (d1, d2) ->
                {
                    start = v.start
                    d1 = d1
                    d2 = d2
                    x = d.value.data
                }
            | _ -> failwith $"Incorrect data type: {v.dataType} for matrix data Protocell."

        member d.unpack() = d.food, d.waste, d.protocell


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
                let domainIntervals = domain.eeDomain.noOfIntervals
                let g i j =
                    match domainIntervals % 2 = 0 with
                    | true -> if ((i + eeShift) * 2 = domainIntervals) && ((j + infShift) * 2 = domainIntervals) then 1.0 else 0.0
                    | false ->
                        if (((i + eeShift) * 2 = domainIntervals - 1) || ((i + eeShift) * 2 = domainIntervals + 1)) && (((j + infShift) * 2 = domainIntervals - 1) || ((j + infShift) * 2 = domainIntervals + 1)) then 1.0 else 0.0

                let v = domain.eeDomain.points.value |> Array.mapi (fun i _ -> domain.infDomain.points.value |> Array.mapi (fun j _ -> g i j)) |> Matrix
                let norm = getNorm v
                (eps / norm) * v

        member p.getU eps (domain : Domain2D) = p.calculateU eps domain.integrateValues domain

        member p.getIntU (uTotal : int64) (domain : Domain2D) =
            let m = p.calculateU (double uTotal) (fun v -> v.total()) domain
            m.convert int64


    type EeInfDiffInitParams =
        {
            eps : double
            protocellInitParams : ProtocellInitParams
            total : double
        }

        static member defaultValue =
            {
                eps = 1.0e-2
                protocellInitParams = ProtocellInitParams.defaultValue
                total = 10.0
            }

        member p.shifted shift = { p with protocellInitParams = ProtocellInitParams.create shift }


    type EeInfDiffModelParams =
        {
            eeInfModelParams : EeInfModelParams
            diffInitParams : EeInfDiffInitParams
        }

        member _.evolutionType = EvolutionType.DifferentialEvolution
        member p.shifted shift = { p with diffInitParams = p.diffInitParams.shifted shift }
        member p.named n = { p with eeInfModelParams = { p.eeInfModelParams with name = Some n } }

        /// Default linear value, mostly for tests, as it does not have many practical purposes.
        static member defaultValue =
            {
                eeInfModelParams = EeInfModelParams.defaultValue
                diffInitParams = EeInfDiffInitParams.defaultValue
            }

        /// Default value with quadratic kernel and non-linear gamma.
        /// This is the main starting point where we can vary k0, eps0, gamma0, etc...
        static member defaultNonLinearValue =
            { EeInfDiffModelParams.defaultValue with eeInfModelParams = EeInfModelParams.defaultNonLinearValue }

        static member withK0 k0 p = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withK0 k0 }
        static member withEps0 eps0 p = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withEps0 eps0 }
        static member withGamma0 gamma0 p = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withGamma0 gamma0 }
        static member withDomainIntervals d p = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withDomainIntervals d }


    type EeInfDiffModel =
        {
            kernelData : KernelData
            gamma : Gamma
            diffInitialValues : SubstanceData
            diffModelParams : EeInfDiffModelParams // To keep all params used to create a model.
        }

        member private md.unpack() =
            let p = md.diffModelParams.eeInfModelParams
            md.kernelData, md.gamma.value, p.numberOfMolecules.value, p.recyclingRate.value

        /// Calculates a derivative.
        member md.derivative (x : SubstanceData) =
            let f, w, u = x.unpack()
            let k, g, n, s = md.unpack()

            let gamma_u = g * u
            let int_gamma_u = k.domain2D.integrateValues gamma_u
            let int_k_u = k.integrateValues u
            let int_int_k_u = k.domain2D.integrateValues int_k_u
            let f_n = (pown f n)

            let df = (double n) * (s * w - f_n * int_int_k_u) |> FoodData
            let dw = - s * w + int_gamma_u |> WasteData
            let du = (f_n * int_k_u - gamma_u) |> ProtocellData

            let retVal = SubstanceData.create df dw du
            retVal

        member md.substanceData i x = LinearData<SubstanceType, double>.create i x |> SubstanceData

        member md.derivativeCalculator f (i : LinearDataInfo<SubstanceType>) =
            let d t x =
                let v = md.substanceData i x
                f t v
                let dx = md.derivative v
                dx.value.data

            FullArray d

        member md.invariant (v : SubstanceData) =
            let f, w, u = v.unpack()
            let k, _, n, _ = md.unpack()

            let int_u = k.domain2D.integrateValues u
            let inv = (double n) * (int_u + w) + f
            inv

        static member create (mp : EeInfDiffModelParams) : EeInfDiffModel =
            let k = KernelData.create mp.evolutionType mp.eeInfModelParams.kernelParams

            let f = FoodData (mp.diffInitParams.total - (double mp.eeInfModelParams.numberOfMolecules.value) * mp.diffInitParams.eps)
            let w = WasteData 0.0
            let u = mp.diffInitParams.protocellInitParams.getU mp.diffInitParams.eps k.domain2D |> ProtocellData
            let sd = SubstanceData.create f w u

            {
                kernelData = k
                gamma = Gamma.create k.domain2D mp.eeInfModelParams.gammaFuncValue
                diffInitialValues = sd
                diffModelParams = mp
            }
