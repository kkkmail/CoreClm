namespace FredholmSolver

open Primitives.VersionInfo
open Primitives.GeneralData
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


    type SubstanceIntData =
        {
            food : FoodIntData
            waste : WasteIntData
            protocell : ProtocellIntData
        }

    type EeInfIntModel =
        {
            kernelData : KernelData
            gamma : Gamma
            intInitialValues : SubstanceIntData
            intModelParams : EeInfModelParams // To keep all params used to create a model.
        }

        member private md.unpack() =
            let p = md.intModelParams
            md.kernelData, md.gamma, p.numberOfMolecules.value, p.recyclingRate.value

        /// Calculates a new state of the system after one epoch.
        /// !!! This is different from a derivative above, which calculates the "difference" between states !!!
        member md.evolve (p : PoissonSampler) (x : SubstanceIntData) =
            let f = x.food.value
            let w = x.waste.value
            let u = x.protocell.value
            let k, _, n, _ = md.unpack()

            // If the amount of food falls below zero, then treat it as exact zero until enough waste is recycled.
            let f_n = (pown (double (max f 0L)) n)
            let gamma_u = md.gamma.evolve p u
            let int_gamma_u = gamma_u.total()
            let int_k_u = k.evolve p f_n u
            let int_int_k_u = int_k_u.total()
            let r = md.intModelParams.recyclingRate.evolve p w

            // Note that the food could be "eaten" beyond zero. If that happens, then it will be treated as exact zero until enough waste is recycled.
            let df = (int64 n) * (r - int_int_k_u)
            let dw = - r + int_gamma_u
            let du = int_k_u - gamma_u

            let f1 = f + df |> FoodIntData
            let w1 = w + dw |> WasteIntData
            let u1 = u + du |> ProtocellIntData

            let retVal =  { food = f1; waste = w1; protocell = u1 }
            retVal

        // member md.invariant (v : SubstanceData) =
        //     let f, w, u = v.unpack()
        //     let k, _, n, _ = md.unpack()
        //
        //     let int_u = k.domain2D.integrateValues u
        //     let inv = (double n) * (int_u + w) + f
        //     inv
        //
        // static member create (mp : EeInfModelParams) : EeInfModel =
        //     let k = KernelData.create mp.kernelParams
        //
        //     let f = FoodData (mp.initParams.total - (double mp.numberOfMolecules.value) * mp.initParams.eps)
        //     let w = WasteData 0.0
        //     let u = mp.initParams.protocellInitParams.getU mp.initParams.eps k.domain2D |> ProtocellData
        //     let sd = SubstanceData.create f w u
        //
        //     {
        //         kernelData = k
        //         gamma = Gamma.create k.domain2D mp.gammaFuncValue
        //         initialValues = sd
        //         modelParams = mp
        //     }
