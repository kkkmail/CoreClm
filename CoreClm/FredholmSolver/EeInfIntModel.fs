namespace FredholmSolver

open FredholmSolver.Primitives
open FredholmSolver.Kernel

module EeInfIntModel =

    type FoodIntData =
        | FoodIntData of int64

        member r.value = let (FoodIntData v) = r in v


    type WasteIntData =
        | WasteIntData of int64

        member r.value = let (WasteIntData v) = r in v


    type ProtocellIntData =
        | ProtocellIntData of Matrix<int64>

        member r.value = let (ProtocellIntData v) = r in v
        member r.total() = r.value.total()


    type SubstanceIntData =
        {
            food : FoodIntData
            waste : WasteIntData
            protocell : ProtocellIntData
        }


    type MoleculeCount =
        | MoleculeCount of int64

        member r.value = let (MoleculeCount v) = r in v
        static member OneThousand = MoleculeCount 1_000L // 10^3
        static member OneMillion = MoleculeCount 1_000_000L // 10^6
        static member OneBillion = MoleculeCount 1_000_000_000L // 10^9
        static member OneTrillion = MoleculeCount 1_000_000_000_000L // 10^12
        static member OneQuadrillion = MoleculeCount 1_000_000_000_000_000L // 10^15
        static member OneQuintillion = MoleculeCount 1_000_000_000_000_000_000L // 10^18


    type EeInfIntInitParams =
        {
            uInitial : MoleculeCount
            protocellInitParams : EeInfDiffModel.ProtocellInitParams
            totalMolecules : MoleculeCount
        }

        static member defaultValue =
            {
                uInitial = MoleculeCount.OneThousand
                protocellInitParams = EeInfDiffModel.ProtocellInitParams.defaultValue
                totalMolecules = MoleculeCount.OneBillion
            }

        member p.shifted shift = { p with protocellInitParams = EeInfDiffModel.ProtocellInitParams.create shift }
        member p.withTotalMolecules totalMolecules = { p with totalMolecules = totalMolecules }
        member p.withUInitial uInitial = { p with uInitial = uInitial }


    type EeInfIntModelParams =
        {
            eeInfModelParams : EeInfModelParams
            intInitParams : EeInfIntInitParams
        }

        member p.shifted shift = { p with intInitParams = p.intInitParams.shifted shift }
        member p.named n = { p with eeInfModelParams = { p.eeInfModelParams with name = Some n } }
        member p.withTotalMolecules totalMolecules = { p with intInitParams = p.intInitParams.withTotalMolecules totalMolecules }
        member p.withUInitial uInitial = { p with intInitParams = p.intInitParams.withUInitial uInitial }

        /// Default linear value, mostly for tests, as it does not have many practical purposes.
        static member defaultValue =
            {
                eeInfModelParams = EeInfModelParams.defaultValue
                intInitParams = EeInfIntInitParams.defaultValue
            }

        /// Default value with quadratic kernel and non-linear gamma.
        /// This is the main starting point where we can vary k0, eps0, gamma0, etc...
        static member defaultNonLinearValue =
            { EeInfIntModelParams.defaultValue with eeInfModelParams = EeInfModelParams.defaultNonLinearValue }

        static member defaultQuadraticWithLinearInfValue =
            { EeInfIntModelParams.defaultValue with eeInfModelParams = EeInfModelParams.defaultQuadraticWithLinearInfValue }

        static member withK0 k0 p = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withK0 k0 }
        static member withKaFunc (ka : KaFuncValue) p = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withKaFunc ka }
        static member withEps0 eps0 p = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withEps0 eps0 }
        static member withGamma0 gamma0 p = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withGamma0 gamma0 }
        static member withDomainIntervals d p = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withDomainIntervals d }


    type EeInfIntModel =
        {
            kernelData : KernelData
            gamma : Gamma
            intInitialValues : SubstanceIntData
            intModelParams : EeInfIntModelParams // To keep all params used to create a model.
        }

        member private md.unpack() =
            let p = md.intModelParams.eeInfModelParams
            md.kernelData, md.gamma, p.numberOfMolecules.value, p.recyclingRate.value

        /// Calculates a new state of the system after one epoch.
        /// !!! This is different from a derivative above, which calculates the "difference" between states !!!
        member md.evolve (p : PoissonSampler) (x : SubstanceIntData) =
            let f = x.food.value
            let w = x.waste.value
            let u = x.protocell.value
            let k, _, n, _ = md.unpack()

            // If the amount of food falls below zero, then treat it as exact zero until enough waste is recycled.
            let r = md.intModelParams.eeInfModelParams.recyclingRate.evolve p w
            let gamma_u = md.gamma.evolve p u
            let int_gamma_u = gamma_u.total()
            let f_n = (pown (double (max f 0L)) n)
            let int_k_u = k.evolve p f_n u
            let int_int_k_u = int_k_u.total()

            // Note that the food could be "eaten" beyond zero. If that happens, then it will be treated as exact zero until enough waste is recycled.
            let df = (int64 n) * (r - int_int_k_u)
            let dw = - r + int_gamma_u
            let du = int_k_u - gamma_u

            if f + df >= 0L then
                let f1 = f + df |> FoodIntData
                let w1 = w + dw |> WasteIntData
                let u1 = u + du |> ProtocellIntData

                let retVal =  { food = f1; waste = w1; protocell = u1 }
                retVal
            else
                // We get here if the food is getting eaten too fast.
                // Here is what we do:
                //   1. Adjust f_n.
                let c = (double int_int_k_u) / f_n
                let f_n1 = max (min (((double f) + (double r) * (double n)) / (c * (double n))) f_n) 0.0

                //   2. Recalculate df and du.
                let int_k_u = k.evolve p f_n1 u
                let int_int_k_u = int_k_u.total()

                let df = (int64 n) * (r - int_int_k_u)
                let du = int_k_u - gamma_u

                let f1 = f + df |> FoodIntData
                let w1 = w + dw |> WasteIntData
                let u1 = u + du |> ProtocellIntData

                let retVal =  { food = f1; waste = w1; protocell = u1 }
                retVal

        member md.invariant (x : SubstanceIntData) =
            let f = x.food.value
            let w = x.waste.value
            let u = x.protocell.value
            let n = md.intModelParams.eeInfModelParams.numberOfMolecules.value

            let int_u = u.total()
            let inv = (int64 n) * (int_u + w) + f
            inv

        static member create (mp : EeInfIntModelParams) : EeInfIntModel =
            let totalMolecules = mp.intInitParams.totalMolecules.value
            let n = mp.eeInfModelParams.numberOfMolecules.value

            // Need to rescale kaFuncValue.
            let kMult = pown (double totalMolecules) n
            let kp = mp.eeInfModelParams.kernelParams
            let k0 = kp.kaFuncValue.k0.value / kMult |> K0
            let kpScaled = { kp with kaFuncValue = kp.kaFuncValue.withK0 k0 }

            let k = KernelData.create kpScaled

            let f = totalMolecules - (int64 n) * mp.intInitParams.uInitial.value |> FoodIntData
            let w = 0L |> WasteIntData
            let u = mp.intInitParams.protocellInitParams.getIntU mp.intInitParams.uInitial.value k.domain2D |> ProtocellIntData

            {
                kernelData = k
                gamma = Gamma.create k.domain2D mp.eeInfModelParams.gammaFuncValue
                intInitialValues =
                    {
                        food = f
                        waste = w
                        protocell = u
                    }
                intModelParams = mp
            }
