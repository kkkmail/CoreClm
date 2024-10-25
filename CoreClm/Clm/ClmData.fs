namespace Clm

open ClmSys.SolverRunnerPrimitives
open Clm.CalculationData
open ModelParams
open ClmSys.ContGenPrimitives

module ClmData =

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
            b : int
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
