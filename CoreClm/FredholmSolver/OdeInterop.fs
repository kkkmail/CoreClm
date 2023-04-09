namespace FredholmSolver

open Softellect.OdePackInterop
open ClmSys.SolverRunnerPrimitives
open FredholmSolver.Primitives
open FredholmSolver.Kernel

module OdeInterop =

    type SubstanceType =
        | Food
        | Waste
        | Protocell of int * int

        member t.linearDataType =
            match t with
            | Food -> ScalarData
            | Waste -> ScalarData
            | Protocell (n1, n2) -> MatrixData (n1, n2)


    type SubstanceData =
        | SubstanceData of LinearData<SubstanceType, double>

        member r.value = let (SubstanceData v) = r in v

        member d.create (x : double) (w: double) (p : Matrix<double>) =
            let data = LinearData<SubstanceType, double>.defaultValue
            let pValue = p.value
            let retVal = data.append(Food, x).append(Waste, w).append(Protocell (pValue.Length, pValue.[0].Length), pValue)
            retVal
