namespace FredholmSolver

open FredholmSolver.Primitives
open FredholmSolver.Kernel
open GenericOdeSolver.Solver

module OdeInterop =

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


    type ModelData =
        {
            kernel : Kernel
            gamma : Matrix<double>
            n : int
            s : double
        }

        member md.derivative (x : SubstanceData) =
            let f = x.food
            let w = x.waste
            let u = x.protocell

            let n = md.n
            let s = md.s

            let gamma_u = md.gamma * u
            let int_k_u = md.kernel.integrateValues u
            let int_int_k_u = md.kernel.domainData.integrateValues int_k_u
            let f_n = (pown f n)

            let df = - (double n) * f_n * int_int_k_u + s * w |> FoodData
            let dw = - s * w + (md.kernel.domainData.integrateValues gamma_u) |> WasteData
            let du = (f_n * int_k_u - gamma_u) |> ProtocellData

            let retVal = SubstanceData.create df dw du
            retVal

        member md.derivativeCalculator (i : LinearDataInfo<SubstanceType>) =
            let d _ x =
                let v = LinearData<SubstanceType, double>.create i x |> SubstanceData
                let dx = md.derivative v
                dx.value.data

            FullArray d


    type FredholmNSolveParam = NSolveParam<int, int, int>
