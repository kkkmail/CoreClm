namespace FredholmSolver

open FredholmSolver.Primitives
open FredholmSolver.Kernel
open FredholmSolver.Common
open Softellect.DistributedProcessing.Primitives.Common
open Softellect.Math.Primitives

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


    type SubstanceLinearData =
        | SubstanceLinearData of LinearData<SubstanceType, double>

        member r.value = let (SubstanceLinearData v) = r in v

        static member create (x : FoodData) (w: WasteData) (p : ProtocellData) =
            let retVal =
                LinearData<SubstanceType, double>.defaultValue
                    .append(Food, x.value)
                    .append(Waste, w.value)
                    .append(Protocell, p.value)
                |> SubstanceLinearData

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
        member p.named n = { p with eeInfModelParams.name = n }

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

        static member withK0 k0 p : EeInfDiffModelParams = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withK0 k0 }
        static member withEps0 eps0 p : EeInfDiffModelParams = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withEps0 eps0 }
        static member withGamma0 gamma0 p : EeInfDiffModelParams = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withGamma0 gamma0 }
        static member withDomainIntervals d p : EeInfDiffModelParams = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withDomainIntervals d }


    type EeInfDiffModel =
        {
            kernelData : KernelData
            gamma : Gamma
            diffInitialValues : SubstanceLinearData
            diffModelParams : EeInfDiffModelParams // To keep all params used to create a model.
        }

        member private md.unpack() =
            let p = md.diffModelParams.eeInfModelParams
            md.kernelData, md.gamma.value, p.numberOfMolecules.value, p.recyclingRate.value

        /// Calculates a derivative.
        member md.derivative (x : SubstanceLinearData) =
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

            let retVal = SubstanceLinearData.create df dw du
            retVal

        member md.substanceData i x = LinearData<SubstanceType, double>.create i x |> SubstanceLinearData

        member md.derivativeCalculator f (i : LinearDataInfo<SubstanceType>) =
            let d t x =
                let v = md.substanceData i x
                f t v
                let dx = md.derivative v
                dx.value.data

            FullArray d

        member md.invariant (v : SubstanceLinearData) =
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
            let sd = SubstanceLinearData.create f w u

            {
                kernelData = k
                gamma = Gamma.create k.domain2D mp.eeInfModelParams.gammaFuncValue
                diffInitialValues = sd
                diffModelParams = mp
            }
