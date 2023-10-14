﻿namespace FredholmSolver

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

        /// Default linear value, mostly for tests, as it does not have many practical purposes.
        static member defaultValue =
            {
                kernelParams = KernelParams.defaultValue
                gammaFuncValue = GammaFuncValue.defaultValue
                numberOfMolecules = NumberOfMolecules.defaultValue
                recyclingRate = RecyclingRate.defaultValue
                initParams = EeInfInitParams.defaultValue
                name = None
            }

        /// Default value with quadratic kernel and non-linear gamma.
        /// This is the main starting point where we can vary k0, eps0, gamma0, etc...
        static member defaultNonLinearValue =
            let kp = KernelParams.defaultQuadraticValue
            let d = kp.domain2D()
            { EeInfModelParams.defaultValue with kernelParams = kp; gammaFuncValue = GammaFuncValue.defaultNonLinearValue d }

        static member withK0 k0 p = { p with kernelParams = p.kernelParams |> KernelParams.withK0 k0 }
        static member withEps0 eps0 p = { p with kernelParams = p.kernelParams |> KernelParams.withEps0 eps0 }
        static member withGamma0 gamma0 p = { p with gammaFuncValue = p.gammaFuncValue |> GammaFuncValue.withGamma0 gamma0 }
        static member withDomainIntervals d p = { p with kernelParams = p.kernelParams |> KernelParams.withDomainIntervals d }


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
            let r = md.modelParams.recyclingRate.evolve p w

            // Note that the food could be "eaten" beyond zero. If that happens, then it will be treated as exact zero until enough waste is recycled.
            let df = (int64 n) * (r - int_int_k_u)
            let dw = - r + int_gamma_u
            let du = int_k_u - gamma_u

            let f1 = f + df |> FoodIntData
            let w1 = w + dw |> WasteIntData
            let u1 = u + du |> ProtocellIntData

            let retVal =  { food = f1; waste = w1; protocell = u1 }
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
