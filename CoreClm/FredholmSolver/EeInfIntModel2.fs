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

    type SubstanceData = SubstanceData<Point2D>
    type EeInfIntInitParams = EeInfIntModel.EeInfIntInitParams
    type EeInfIntModelParams = EeInfIntModel.EeInfIntModelParams

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


    type EeInfIntModel =
        {
            // kernelData : KernelData
            // gamma : Gamma
            model : SimpleEvolutionModel<Point2D, Coord2D>
            evolutionContext : EvolutionContext<Point2D, int64>
            intInitialValues : SubstanceData
            intModelParams : EeInfIntModelParams // To keep all params used to create a model.
            domain2D : Domain2D
        }

        // member private md.unpack() =
        //     let p = md.intModelParams.eeInfModelParams
        //     md.kernelData, md.gamma, p.numberOfMolecules.value, p.recyclingRate.value

        member md.modelString = md.intModelParams.modelString

        /// Calculates a new state of the system after one epoch.
        /// !!! This is different from a derivative above, which calculates the "difference" between states !!!
        member md.evolve (x : SubstanceData) = md.model.evolveStep md.evolutionContext x

        member md.invariant (x : SubstanceData) = md.model.invariant x

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
            // TODO kk:20250325 - Tridiagonal matrices need to be updated to use different probabilities in different directions.
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
                domain2D = domain
            }
