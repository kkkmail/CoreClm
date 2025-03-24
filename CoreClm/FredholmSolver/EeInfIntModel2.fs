namespace FredholmSolver

// open FredholmSolver.Primitives
// open FredholmSolver.Kernel

open System
open Softellect.Sys.Primitives
open Softellect.Analytics.Wolfram
open Softellect.Sys.Core

open Softellect.Math.Primitives
open Softellect.Math.Sparse
open Softellect.Math.Tridiagonal
open Softellect.Math.Evolution
open Softellect.Math.Models

module EeInfIntModel2 =
    // Aliases to avoid opening the whole FredholmSolver.Primitives module.
    let toModelStringInt64 = Primitives.toModelStringInt64
    let bindPrefix = Primitives.bindPrefix

    // Aliases to avoid opening the whole FredholmSolver.Kernel module.
    type EeInfModelParams = Kernel.EeInfModelParams
    type KaFuncValue = Kernel.KaFuncValue
    type K0 = Kernel.K0

    // type PoissonSampler = Softellect.Math.Sampling.PoissonSampler

    // ==========================================

    type Kernel.GammaFuncValue
        with
        member g.gammaFunc2 (d : Domain2D) =
            match g with
            | Kernel.ScalarGamma e -> (fun _ -> e)
            | Kernel.SeparableGamma e ->
                (fun (p : Point2D) -> e.eeInfScale * (Kernel.separableFunc e.tEeInf d.d0.points.value[p.i0] d.d1.points.value[p.i1]))


    type Kernel.KaFuncValue
        with
        member k.kaFunc2 (d : Domain2D) =
            match k with
            | Kernel.IdentityKa e -> (fun _ -> e)
            | Kernel.SeparableKa e ->
                (fun (p : Point2D) -> e.eeInfScale * (Kernel.separableFunc e.tEeInf d.d0.points.value[p.i0] d.d1.points.value[p.i1]))


    type Kernel.EpsFuncValue
        with
        member e.eps0 =
            match e with
            | Kernel.ScalarEps e0 -> e0

    // ==========================================

    type SolutionMethod =
        | Euler    // https://en.wikipedia.org/wiki/Euler_method
        | MidPoint // https://en.wikipedia.org/wiki/Midpoint_method - Not implemented yet.


    /// All parameterization of the solution method.
    type SolutionMethodParams =
        {
            solutionMethod : SolutionMethod
            useParallel : bool
        }

        static member defaultValue =
            {
                solutionMethod = Euler
                useParallel = false
            }

        member s.modelString =
            let sm =
                match s.solutionMethod with
                | Euler -> EmptyString
                | MidPoint -> "_mp"

            let p = if s.useParallel then "_p" else EmptyString
            sm + p


    // type NoOfEpochs =
    //     | NoOfEpochs of int
    //
    //     member r.value = let (NoOfEpochs v) = r in v


    // type FoodIntData =
    //     | FoodIntData of int64
    //
    //     member r.value = let (FoodIntData v) = r in v
    //
    //
    // type WasteIntData =
    //     | WasteIntData of int64
    //
    //     member r.value = let (WasteIntData v) = r in v
    //
    //
    // type ProtocellIntData =
    //     | ProtocellIntData of Matrix<int64>
    //
    //     member r.value = let (ProtocellIntData v) = r in v
    //     member r.total() = r.value.total()


    type SubstanceIntData = SubstanceData<Point2D>
        // {
        //     food : FoodIntData
        //     waste : WasteIntData
        //     protocell : ProtocellIntData
        // }


    // type MoleculeCount =
    //     | MoleculeCount of int64
    //
    //     member r.value = let (MoleculeCount v) = r in v
    //     static member OneThousand = MoleculeCount 1_000L // 10^3 - K
    //     static member OneMillion = MoleculeCount 1_000_000L // 10^6 - M
    //     static member OneBillion = MoleculeCount 1_000_000_000L // 10^9 - G
    //     static member OneTrillion = MoleculeCount 1_000_000_000_000L // 10^12 - T
    //     static member OneQuadrillion = MoleculeCount 1_000_000_000_000_000L // 10^15 - P
    //     static member OneQuintillion = MoleculeCount 1_000_000_000_000_000_000L // 10^18 - E


    type EeInfIntInitParams =
        {
            uInitial : MoleculeCount
            protocellInitParams : EeInfDiffModel.ProtocellInitParams
            totalMolecules : MoleculeCount
            seedValue : int
        }

        static member defaultValue =
            {
                uInitial = MoleculeCount.OneThousand
                protocellInitParams = EeInfDiffModel.ProtocellInitParams.defaultValue
                totalMolecules = MoleculeCount.OneBillion
                seedValue = 1
            }

        member p.modelString =
            let df = EeInfIntInitParams.defaultValue
            let f =  toModelStringInt64 df.totalMolecules.value p.totalMolecules.value |> bindPrefix "f"
            let u =  toModelStringInt64 df.uInitial.value p.uInitial.value |> bindPrefix "u"
            let s = p.protocellInitParams.modelString
            let r =  if df.seedValue = p.seedValue then None else Some p.seedValue |> bindPrefix "f"
            [| f; u; s; r |] |> Array.choose id |> joinStrings EmptyString

        member p.shifted shift = { p with protocellInitParams = EeInfDiffModel.ProtocellInitParams.create shift }
        member p.withTotalMolecules totalMolecules = { p with totalMolecules = totalMolecules }
        member p.withUInitial uInitial = { p with uInitial = uInitial }


    type EeInfIntModelParams =
        {
            eeInfModelParams : EeInfModelParams
            intInitParams : EeInfIntInitParams
            solutionMethodParams : SolutionMethodParams
        }

        member _.evolutionType = EvolutionType.DiscreteEvolution
        member p.shifted shift = { p with intInitParams = p.intInitParams.shifted shift }
        member p.named n = { p with eeInfModelParams.name = n }
        member p.withTotalMolecules totalMolecules = { p with intInitParams = p.intInitParams.withTotalMolecules totalMolecules }
        member p.withUInitial uInitial = { p with intInitParams = p.intInitParams.withUInitial uInitial }
        member p.withInfMaxValue infMaxValue = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withInfMaxValue infMaxValue }
        member p.withProtocellInitParams protocellInitParams = { p with intInitParams = { p.intInitParams with protocellInitParams = protocellInitParams } }
        member p.modelString = $"{p.eeInfModelParams.modelString}{p.solutionMethodParams.modelString}{p.intInitParams.modelString}"

        /// Default linear value, mostly for tests, as it does not have many practical usages.
        static member defaultValue =
            {
                eeInfModelParams = EeInfModelParams.defaultValue
                intInitParams = EeInfIntInitParams.defaultValue
                solutionMethodParams = SolutionMethodParams.defaultValue
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
        static member withGlobalAsymmetryFactor a p = { p with eeInfModelParams = p.eeInfModelParams |> EeInfModelParams.withGlobalAsymmetryFactor a }


    type EeInfIntModel =
        {
            // kernelData : KernelData
            // gamma : Gamma
            model : SimpleEvolutionModel<Point2D, Coord2D>
            evolutionContext : EvolutionContext<Point2D, int64>
            intInitialValues : SubstanceIntData
            intModelParams : EeInfIntModelParams // To keep all params used to create a model.
        }

        // member private md.unpack() =
        //     let p = md.intModelParams.eeInfModelParams
        //     md.kernelData, md.gamma, p.numberOfMolecules.value, p.recyclingRate.value

        member md.modelString = md.intModelParams.modelString

        /// Calculates a new state of the system after one epoch.
        /// !!! This is different from a derivative above, which calculates the "difference" between states !!!
        member md.evolve (x : SubstanceIntData) = md.model.evolveStep md.evolutionContext x

        // member md.evolve (p : PoissonSampler) (x : SubstanceIntData) =
        //     let f = x.food.value
        //     let w = x.waste.value
        //     let u = x.protocell.value
        //     let k, _, n, _ = md.unpack()
        //     let s = p.sampler
        //     let useParallel = md.intModelParams.solutionMethodParams.useParallel
        //
        //     // If the amount of food falls below zero, then treat it as exact zero until enough waste is recycled.
        //     let r = md.intModelParams.eeInfModelParams.recyclingRate.evolve s w
        //     let gamma_u = md.gamma.evolve s u
        //     let int_gamma_u = gamma_u.total()
        //     let f_n = (pown (double (max f 0L)) n)
        //     let int_k_u = k.evolve useParallel p f_n u
        //     let int_int_k_u = int_k_u.total()
        //
        //     // Note that the food could be "eaten" beyond zero. If that happens, then it will be treated as exact zero until enough waste is recycled.
        //     let df = (int64 n) * (r - int_int_k_u)
        //     let dw = - r + int_gamma_u
        //     let du = int_k_u - gamma_u
        //
        //     if f + df >= 0L then
        //         let f1 = f + df |> FoodIntData
        //         let w1 = w + dw |> WasteIntData
        //         let u1 = u + du |> ProtocellIntData
        //
        //         let retVal =  { food = f1; waste = w1; protocell = u1 }
        //         retVal
        //     else
        //         // We get here if the food is getting eaten too fast.
        //         // Here is what we do:
        //         //   1. Adjust f_n.
        //         let c = (double int_int_k_u) / f_n
        //         let f_n1 = max (min (((double f) + (double r) * (double n)) / (c * (double n))) f_n) 0.0
        //
        //         //   2. Recalculate df and du.
        //         let int_k_u = k.evolve useParallel p f_n1 u
        //         let int_int_k_u = int_k_u.total()
        //
        //         let df = (int64 n) * (r - int_int_k_u)
        //         let du = int_k_u - gamma_u
        //
        //         let f1 = f + df |> FoodIntData
        //         let w1 = w + dw |> WasteIntData
        //         let u1 = u + du |> ProtocellIntData
        //
        //         let retVal =  { food = f1; waste = w1; protocell = u1 }
        //         retVal

        member md.invariant (x : SubstanceIntData) = md.model.invariant x

        static member create (mp : EeInfIntModelParams) : EeInfIntModel =
            let totalMolecules = MoleculeCount mp.intInitParams.totalMolecules.value
            let n = NumberOfMolecules mp.eeInfModelParams.numberOfMolecules.value
            let d = DomainIntervals mp.eeInfModelParams.kernelParams.domainIntervals.value

            // Need to rescale kaFuncValue.
            let kMult = pown (double totalMolecules.value) n.value
            let kp = mp.eeInfModelParams.kernelParams
            let k0 = kp.kaFuncValue.k0.value / kMult |> K0.K0
            let kpScaled = { kp with kaFuncValue = kp.kaFuncValue.withK0 k0 }
            let e0 = mp.eeInfModelParams.kernelParams.epsEeFuncValue.eps0.value

            let domain =
                {
                    d0 = Domain.create(d, DomainRange.defaultValue) // ee domain
                    d1 = Domain.create(d, { minValue = 0.0; maxValue = mp.eeInfModelParams.kernelParams.infMaxValue.value }) // inf domain
                }

            // let k = Kernel.KernelData.create mp.evolutionType kpScaled
            // let gamma = Kernel.Gamma.create k.domain2D mp.eeInfModelParams.gammaFuncValue

            let f = totalMolecules.value - (int64 n.value) * mp.intInitParams.uInitial.value |> FoodData
            let w = 0L |> WasteData

            // Legacy domain.
            // let domain2D = Kernel.Domain2D.create mp.eeInfModelParams.kernelParams.domainIntervals.value mp.eeInfModelParams.kernelParams.infMaxValue.value

            let u =
                (mp.intInitParams.protocellInitParams.getIntU mp.intInitParams.uInitial.value domain).value
                |> Array.mapi (fun i v -> v |> Array.mapi (fun j e -> { x = { i0 = i; i1 = j }; value = e}))
                |> Array.concat
                |> SparseArray.create
                |> ProtoCellData

            // TODO kk:20250325 - This needs to be adjusted to account that eps is for Gaussian distribution.
            // TODO kk:20250325 - Tridiagonal matriicex need to be updated to use different probabilities in different directions.
            let a = (1.0 - e0)

            let gammaFunc = mp.eeInfModelParams.gammaFuncValue.gammaFunc2 domain
            let gamma : Multiplier<Point2D> = Multiplier gammaFunc
            // let kaFunc0 = mp.eeInfModelParams.kernelParams.kaFuncValue.kaFunc domain
            let kaFunc = kpScaled.kaFuncValue.kaFunc2 domain
            let multiplier : Multiplier<Point2D> = Multiplier kaFunc
            let evolutionMatrix : SparseMatrix<Point2D, double> = createTridiagonalMatrix2D d.value a
            let ps = Random mp.intInitParams.seedValue |> PoissonSampler.create int64
            let poissonSampler : PoissonSampler<int64> = ps

            let model =
                {
                    replication =
                        {
                            multiplier = multiplier
                            evolutionMatrix = evolutionMatrix
                        }

                    decay = gamma
                    recyclingRate = RecyclingRate mp.eeInfModelParams.recyclingRate.value
                    numberOfMolecules = n
                    converter = conversionParameters2D domain
                }

            let ec =
                {
                    poissonSampler = poissonSampler
                    toDouble = double
                    fromDouble = int64
                }

            {
                // kernelData = k
                // gamma = Gamma.create k.domain2D mp.eeInfModelParams.gammaFuncValue
                model = model
                evolutionContext = ec
                intInitialValues =
                    {
                        food = f
                        waste = w
                        protocell = u
                    }
                intModelParams = mp
            }
