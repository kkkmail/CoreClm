namespace FredholmSolver

open FredholmSolver.Primitives

module Kernel =

    type MutationProbabilityData =
        {
            domain : Domain
            zeroThreshold : double
            epsFunc : double -> double
        }

        static member defaultZeroThreshold : double = 1.0e-05


    /// Creates a normalized mutation probability.
    /// The normalization is performed using integral estimate over the domain.
    type MutationProbability =
        | MutationProbability of SparseArray<double>

        member r.value = let (MutationProbability v) = r in v

        /// m is a real (unscaled) value but e is scaled to the half of the range.
        static member create (data : MutationProbabilityData) mean =
            let epsFunc x = (data.epsFunc x) * data.domain.range / 2.0
            let f x : double = exp (- pown ((x - mean) / (epsFunc x)) 2)
            let values = data.domain.midPoints |> Array.map f |> SparseArray<double>.create data.zeroThreshold
            let norm = data.domain.integrateValues values
            let p = values.value |> Array.map (fun v -> { v with value1D = v.value1D / norm }) |> SparseArray.SparseArray |> MutationProbability
            p


    type MutationProbabilityData2D =
        {
            eeMutationProbabilityData : MutationProbabilityData
            infMutationProbabilityData : MutationProbabilityData
        }

        member d.domain2D =
            {
                eeDomain = d.eeMutationProbabilityData.domain
                infDomain = d.infMutationProbabilityData.domain
            }


    type MutationProbability2D =
        | MutationProbability2D of SparseArray2D<double>

        member r.value = let (MutationProbability2D v) = r in v

        static member create (data : MutationProbabilityData2D) eeMean infMean =
            let p1 = MutationProbability.create data.eeMutationProbabilityData eeMean
            let p2 = MutationProbability.create data.infMutationProbabilityData infMean
            let p = cartesianMultiply p1.value p2.value // |> MutationProbability2D
            p


    /// Constructs p(x, y, x1, y1) sparse array where each numerical integral by (x, y) for each of (x1, y1) should be 1.
    /// Integration by (x, y) is "inconvenient" as normally we'd need to integrate by (x1, y1) and so the structure is
    /// optimized for that.
    type MutationProbability4D =
        {
            x1y1_xy : SparseArray2D<double>[][] // For integration over (x, y)
            xy_x1y1 : SparseArray2D<double>[][] // For "standard" integration over (x1, y1)
        }

        static member create (data : MutationProbabilityData2D) =
            let eeMu = data.domain2D.eeDomain.midPoints
            let infMu = data.domain2D.infDomain.midPoints

            // These are the values where integration by (x, y) should yield 1 for each (x1, y1).
            // So the SparseArray2D[][] is by (x1, y1) and the underlying SparseArray2D is by (x, y).
            let x1y1_xy = eeMu |> Array.map(fun a -> infMu |> Array.map (MutationProbability2D.create data a))

            // Here we need to swap (x, y) <-> (x1, y1) "indexes".
            //
            // let find i j : SparseArray2D =
            //     x1y1_xy
            //
            // let xy_x1y1 =
            //     eeMu
            //     |> Array.mapi (fun i _ -> infMu |> Array.mapi (fun j _ -> find i j))

            let xy_x1y1_Map =
                [| for i in 0..(eeMu.Length - 1) -> [| for j in 0..(infMu.Length - 1) -> SparseValue4D.createArray i j (x1y1_xy[i][j]) |] |]
                |> Array.concat
                |> Array.concat
                |> Array.groupBy (fun e -> e.i1, e.j1)
                |> Array.map (fun (a, b) -> a, b |> Array.map (fun e -> { i = e.i; j = e.j; value2D = e.value4D }) |> Array.sortBy (fun e -> e.i, e.j) |> SparseArray2D<double>.SparseArray2D)
                |> Map.ofArray

            let xy_x1y1 =
                eeMu
                |> Array.mapi (fun i _ -> infMu |> Array.mapi (fun j _ -> xy_x1y1_Map[(i, j)]))

            {
                x1y1_xy = x1y1_xy
                xy_x1y1 = xy_x1y1
            }

    type KernelData =
        {
            noOfIntervals : int
            l2 : double
            epsEe : double -> double
            epsInf : double -> double
        }


    type KernelValue =
        | KernelValue of SparseArray2D<double>[][]

        member r.value = let (KernelValue v) = r in v


    /// Represents K(x, x1, y, y1) 2D Fredholm kernel.
    /// It is convenient to store it as [][] arrays in [][] array,
    /// where the first two indexes are (x, y) and last ones are (x1, y1).
    /// So the actual indexes here are K(x, y, x1, y1)
    type Kernel =
        {
            kernel : KernelValue
            domainData : Domain2D
        }

        member k.integrateValues (u : XY) =
            let v =
                k.kernel.value
                |> Array.map (fun e -> e |> Array.map (fun x -> k.domainData.integrateValues (x, u)))

            v

        static member defaultKernelFunc domainData data x1 y1 : XY =
            let v =
                domainData.eeDomain.midPoints
                |> Array.map (fun x -> domainData.infDomain.midPoints |> Array.map (fun y -> 1.0))
                |> XY.create

            v

        static member create data kernelFunc =
            let domainData = Domain2D.create data.noOfIntervals data.l2

            let kernel =
                domainData.eeDomain.midPoints
                |> Array.map (fun x -> domainData.infDomain.midPoints |> Array.map (kernelFunc domainData data x))
                |> KernelValue

            // let kernelData = kernelFunc data

            {
                kernel = kernel
                domainData = domainData
            }
