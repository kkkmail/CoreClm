namespace FredholmSolver

open Primitives.VersionInfo
open Primitives.GeneralData
open FredholmSolver.Primitives
open FredholmSolver.Kernel
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


    /// We can only shift delta to a grid cell.
    type ProtocellInitParams =
        | DeltaEeShifted of int

        static member create shift = DeltaEeShifted shift
        static member defaultValue = ProtocellInitParams.create 0

        /// Creates a "delta" function centered near (0, 0) in the domain,
        /// which is a middle point in ee domain and 0-th point in inf domain.
        member p.getU eps (domain : Domain2D) =
            match p with
            | DeltaEeShifted eeShift ->
                let domainIntervals = domain.eeDomain.noOfIntervals
                let g i j =
                    match domainIntervals % 2 = 0 with
                    | true -> if ((i + eeShift) * 2 = domainIntervals) && (j = 0) then 1.0 else 0.0
                    | false ->
                        if (((i + eeShift) * 2 = domainIntervals - 1) || ((i + eeShift) * 2 = domainIntervals + 1)) && (j = 0) then 1.0 else 0.0

                let v = domain.eeDomain.points.value |> Array.mapi (fun i _ -> domain.infDomain.points.value |> Array.mapi (fun j _ -> g i j)) |> Matrix
                let norm = domain.integrateValues v
                (eps / norm) * v


    type EeInfInitParams =
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


    type EeInfModelParams =
        {
            kernelParams : KernelParams
            gammaFuncValue : GammaFuncValue
            numberOfMolecules : NumberOfMolecules
            recyclingRate : RecyclingRate
            initParams : EeInfInitParams
            name : string option
        }

        member p.shifted shift = { p with initParams = p.initParams.shifted shift }

        static member defaultValue =
            {
                kernelParams = KernelParams.defaultValue
                gammaFuncValue = GammaFuncValue.defaultValue
                numberOfMolecules = NumberOfMolecules.defaultValue
                recyclingRate = RecyclingRate.defaultValue
                initParams = EeInfInitParams.defaultValue
                name = None
            }

        static member defaultNarrowValue =
            {
                kernelParams = KernelParams.defaultNarrowValue
                gammaFuncValue = GammaFuncValue.defaultValue
                numberOfMolecules = NumberOfMolecules.defaultValue
                recyclingRate = RecyclingRate.defaultValue
                initParams = EeInfInitParams.defaultValue
                name = None
            }

        static member defaultNonlinearValue =
            let kp = KernelParams.defaultQuadraticValue
            let d = kp.domain2D()

            {
                kernelParams = kp
                gammaFuncValue = GammaFuncValue.defaultNonlinearValue d
                numberOfMolecules = NumberOfMolecules.defaultValue
                recyclingRate = RecyclingRate.defaultValue
                initParams = EeInfInitParams.defaultValue
                name = None
            }

        static member defaultNonlinearValue2 =
            let kp = KernelParams.defaultQuadraticValue
            let d = kp.domain2D()

            {
                kernelParams = kp
                gammaFuncValue = GammaFuncValue.defaultNonlinearValue2 d
                numberOfMolecules = NumberOfMolecules.defaultValue2
                recyclingRate = RecyclingRate.defaultValue
                initParams = EeInfInitParams.defaultValue
                name = None
            }


    type EeInfModel =
        {
            kernelData : KernelData
            gamma : Gamma
            initialValues : SubstanceData
            modelParams : EeInfModelParams // To keep all params used to create a model.
        }

        member private md.unpack() =
            let p = md.modelParams
            md.kernelData, md.gamma.value, p.numberOfMolecules.value, p.recyclingRate.value

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
            let k, _, n, _ = md.unpack()

            let int_u = k.domain2D.integrateValues u
            let inv = (double n) * (int_u + w) + f
            inv

        static member create (mp : EeInfModelParams) : EeInfModel =
            let k = KernelData.create mp.kernelParams

            let f = FoodData (mp.initParams.total - (double mp.numberOfMolecules.value) * mp.initParams.eps)
            let w = WasteData 0.0
            let u = mp.initParams.protocellInitParams.getU mp.initParams.eps k.domain2D |> ProtocellData
            let sd = SubstanceData.create f w u

            {
                kernelData = k
                gamma = Gamma.create k.domain2D mp.gammaFuncValue
                initialValues = sd
                modelParams = mp
            }
