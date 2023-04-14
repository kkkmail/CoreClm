namespace FredholmSolverTests

open System
open FredholmSolver.OdeInterop
open GenericOdeSolver.Solver
open Primitives.GeneralPrimitives
open Primitives.SolverPrimitives
open FredholmSolver.Primitives
open FredholmSolver.Kernel
open FluentAssertions
open Xunit
open Xunit.Abstractions

/// TODO kk:20230413 - Consolidate data creation into some common place.
type OdeTests (output : ITestOutputHelper) =
    let writeLine s = output.WriteLine s
    let nullString : string = null
    let errTolerance = 1.0e-10

    let defaultKernelData =
        {
            noOfIntervals = 101
            l2 = 25
            zeroThreshold = MutationProbabilityData.defaultZeroThreshold
            epsEeFunc = (fun _ -> 0.02) |> EpsFunc
            epsInfFunc = (fun _ -> 0.02) |> EpsFunc
            kaFunc = (fun _ _ _ -> 1.0) |> KaFunc
        }

    let domain2D data = Domain2D.create data.noOfIntervals data.l2

    /// Creates a "delta" function centered near (0, 0) in the domain,
    /// which is a middle point in ee domain and 0-th point in inf domain.
    let getDeltaU data =
        let domain =
            if data.noOfIntervals % 2 = 1
            then domain2D data
            else failwith "data.noOfIntervals must be odd for this method to work."

        let g i j = if (i * 2 + 1 = data.noOfIntervals) && (j = 0) then 1.0 else 0.0
        let v = domain.eeDomain.midPoints.value |> Array.mapi (fun i _ -> domain.infDomain.midPoints.value |> Array.mapi (fun j _ -> g i j)) |> Matrix
        let norm = domain.integrateValues v
        (1.0 / norm) * v

    let initialValues data =
        let f = FoodData 0.0
        let w = WasteData 0.0
        let u = getDeltaU data |> ProtocellData
        let sd = SubstanceData.create f w u
        sd


    let modelData data : ModelData =
        let kernel = Kernel.create data

        let gamma =
            kernel.domainData.eeDomain.midPoints.value
            |> Array.map (fun a -> kernel.domainData.infDomain.midPoints.value |> Array.map (fun b -> 0.0))
            |> Matrix

        {
            kernel = kernel
            gamma = gamma
            n = 0
            s = 0.01
        }


    let odeParams =
        {
            startTime = 0.0
            endTime = 1.0
            stepSize = 1.0e-3
            absoluteTolerance = AbsoluteTolerance.defaultValue
            noOfOutputPoints = 1
            noOfProgressPoints = 1
            noOfChartDetailedPoints = None
        }

    let nSolveData data =
        let i = initialValues data
        let md = modelData data

        {
            odeParams = odeParams
            solverType = OdePack (Bdf, ChordWithDiagonalJacobian, UseNonNegative)
            modelDataId = Guid()
            runQueueId = Guid() |> RunQueueId
            initialValues = i.value.data
            getDerivative = md.derivativeCalculator i.value.dataInfo
            checkFreq = TimeSpan.MaxValue
            checkCancellation = fun _ -> "Calculation aborted" |> Some |> CancellationType.AbortCalculation |> Some
        }
    let nSolveParam data =
        {
            nSolveData = nSolveData data

            defaultProgressData = ProgressData<int>.defaultValue 0
            defaultChartSliceData = 0
            defaultExtraData = 0

            progressCallBack = fun _ _ -> ()
            chartCallBack = fun _ -> ()
            getChartSliceData = fun _ _ _ -> 0
            getExtraData = fun () -> 0
        }


    [<Fact>]
    member _.ode_ShouldRun () : unit =
        let nSolveParam = nSolveParam defaultKernelData
        let result = nSolve nSolveParam
        result.Should().NotBeNull(nullString) |> ignore
