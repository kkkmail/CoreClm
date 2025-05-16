namespace FredholmSolver

open FredholmSolver.Primitives
open FredholmSolver.Kernel
open FredholmSolver.Common
open Softellect.Math.Primitives
open Softellect.Math.Models

module EeInfIntModel =

    type ProtocellIntData =
        | ProtocellIntData of Matrix<int64>

        member r.value = let (ProtocellIntData v) = r in v
        member r.total() = r.value.total()


    type SubstanceIntData =
        {
            food : FoodData
            waste : WasteData
            protocell : ProtocellIntData
        }


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

        member md.modelString = md.intModelParams.modelString

        /// Calculates a new state of the system after one epoch.
        /// !!! This is different from a derivative above, which calculates the "difference" between states !!!
        member md.evolve (p : PoissonSampler) (x : SubstanceIntData) =
            let f = x.food.value
            let w = x.waste.value
            let u = x.protocell.value
            let k, _, n, _ = md.unpack()
            let s = p.sampler
            let useParallel = md.intModelParams.solutionMethodParams.useParallel

            // If the amount of food falls below zero, then treat it as exact zero until enough waste is recycled.
            let r = md.intModelParams.eeInfModelParams.recyclingRate.evolve s w
            let gamma_u = md.gamma.evolve s u
            let int_gamma_u = gamma_u.total()
            let f_n = (pown (double (max f 0L)) n)
            let int_k_u = k.evolve useParallel p f_n u
            let int_int_k_u = int_k_u.total()

            // Note that the food could be "eaten" beyond zero. If that happens, then it will be treated as exact zero until enough waste is recycled.
            let df = (int64 n) * (r - int_int_k_u)
            let dw = - r + int_gamma_u
            let du = int_k_u - gamma_u

            if f + df >= 0L then
                let f1 = f + df |> FoodData
                let w1 = w + dw |> WasteData
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
                let int_k_u = k.evolve useParallel p f_n1 u
                let int_int_k_u = int_k_u.total()

                let df = (int64 n) * (r - int_int_k_u)
                let du = int_k_u - gamma_u

                let f1 = f + df |> FoodData
                let w1 = w + dw |> WasteData
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

            let k = KernelData.create mp.evolutionType kpScaled

            let f = totalMolecules - (int64 n) * mp.intInitParams.uInitial.value |> FoodData
            let w = 0L |> WasteData
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
