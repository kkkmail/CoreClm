namespace Clm

open ClmSys.SolverRunnerPrimitives
open Clm.CalculationData
open ModelParams
open ClmSys.ContGenPrimitives
open Softellect.DistributedProcessing.Primitives.Common

module ClmData =

    /// Treat all values of u less than this as zero.
    let correctionValue = 1.0e-12


    /// That's 'I in the type signature.
    type ClmInitialData =
        {
            defaultValueId : ClmDefaultValueId
            modelCommandLineParam : ModelCommandLineParam
            modelData : ModelData
        }


    /// That's 'D in the type signature.
    type ClmSolverContext =
        {
            derivativeCalculator : DerivativeCalculator
            evolutionTime : EvolutionTime
            initialValues : double[]
            // Add information about chartLabels.
        }

        member d.inputParams =
            {
                startTime = EvolutionTime 0m
                endTime = d.evolutionTime
            }

        member d.odeContext =
            {
                stepSize = 0.0
                absoluteTolerance = AbsoluteTolerance.defaultValue
                odeSolverType = OdePack (Bdf, ChordWithDiagonalJacobian, UseNonNegative correctionValue)
                derivative = d.derivativeCalculator
            }


    /// That's 'P in the type signature.
    type ClmProgressData =
        {
            eeData : EeData
        }

        static member defaultValue =
            {
                eeData = EeData.defaultValue
            }


    /// That's 'C in the type signature.
    type ClmChartData =
        {
            d : int
        }
