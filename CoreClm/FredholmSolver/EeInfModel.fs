namespace FredholmSolver

open Primitives.VersionInfo
open Primitives.GeneralData
open FredholmSolver.Primitives
open FredholmSolver.Kernel
open GenericOdeSolver.Solver
open GenericOdeSolver.Primitives

module EeInfModel =

    [<Literal>]
    let DefaultRootFolder = DefaultRootDrive + @":\" + ContGenBaseName + @"\Clm\"

    [<Literal>]
    let DefaultResultLocationFolder = DefaultRootFolder + "Results"

    [<Literal>]
    let DefaultFileStorageFolder = DefaultRootFolder + "FileStorage"


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


    type EeInfModelParams =
        {
            kernelData : KernelData
            gammaFuncValue : GammaFuncValue
            numberOfMolecules : NumberOfMolecules
            recyclingRate : RecyclingRate
        }

        static member defaultValue =
            {
                kernelData = KernelData.defaultValue
                gammaFuncValue = GammaFuncValue.defaultValue
                numberOfMolecules = NumberOfMolecules.defaultValue
                recyclingRate = RecyclingRate.defaultValue
            }

        static member defaultNarrowValue =
            {
                kernelData = KernelData.defaultNarrowValue
                gammaFuncValue = GammaFuncValue.defaultValue
                numberOfMolecules = NumberOfMolecules.defaultValue
                recyclingRate = RecyclingRate.defaultValue
            }

        static member defaultNonlinearValue d =
            {
                kernelData = KernelData.defaultQuadraticValue d
                gammaFuncValue = GammaFuncValue.defaultNonlinearValue d
                numberOfMolecules = NumberOfMolecules.defaultValue
                recyclingRate = RecyclingRate.defaultValue
            }

        static member defaultNonlinearValue2 d =
            {
                kernelData = KernelData.defaultQuadraticValue2 d
                gammaFuncValue = GammaFuncValue.defaultNonlinearValue2 d
                numberOfMolecules = NumberOfMolecules.defaultValue
                recyclingRate = RecyclingRate.defaultValue
            }


    type EeInfModel =
        {
            kernel : Kernel
            gamma : Gamma
            modelParams : EeInfModelParams // To keep all params used to create a model.
        }

        member private md.unpack() =
            let p = md.modelParams
            md.kernel, md.gamma.value, p.numberOfMolecules.value, p.recyclingRate.value

        member md.derivative (x : SubstanceData) =
            let f, w, u = x.unpack()
            let k, g, n, s = md.unpack()

            let gamma_u = g * u
            let int_k_u = k.integrateValues u
            let int_int_k_u = k.domain2D.integrateValues int_k_u
            let f_n = (pown f n)

            let df = (double n) * (s * w - f_n * int_int_k_u)  |> FoodData
            let dw = - s * w + (k.domain2D.integrateValues gamma_u) |> WasteData
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
            let k, g, n, s = md.unpack()

            let int_u = k.domain2D.integrateValues u
            let inv = (double n) * (int_u + w) + f
            inv

        static member create (mp : EeInfModelParams) : EeInfModel =
            let kernel = Kernel.create mp.kernelData

            {
                kernel = kernel
                gamma = Gamma.create kernel.domain2D mp.gammaFuncValue
                modelParams = mp
            }


    type ShiftedDeltaParams =
        {
            eeShift : double
            infShift : double
        }

    type ProtocellInitParams =
        | Delta
        | ShiftedDelta of ShiftedDeltaParams


    type EeInfInitParams =
        {
            eps : double
            protocellInitParams : ProtocellInitParams
            total : double
        }


    type EeInfModelData =
        {
            modelParams : EeInfModelParams
            initParams : EeInfInitParams
        }

        static member defaultValue =
            {
                modelParams = EeInfModelParams.defaultValue
                initParams =
                    {
                        eps = 1.0e-2
                        protocellInitParams = Delta
                        total = 10.0
                    }
            }

        static member defaultNonlinearValue =
            let data = EeInfModelData.defaultValue
            let domain2D = Domain2D.create data.modelParams.kernelData.domainIntervals.value data.modelParams.kernelData.infMaxValue.value
            { EeInfModelData.defaultValue with modelParams = EeInfModelParams.defaultNonlinearValue domain2D }

        static member defaultNonlinearValue2 =
            let data = EeInfModelData.defaultValue
            let domain2D = Domain2D.create data.modelParams.kernelData.domainIntervals.value data.modelParams.kernelData.infMaxValue.value
            { EeInfModelData.defaultValue with modelParams = EeInfModelParams.defaultNonlinearValue2 domain2D }
