namespace Clm

open ClmSys.SolverRunnerPrimitives

module ClmData =

    /// That's 'I in the type signature.
    type ClmImitialData =
        {
            a : int
        }


    /// That's 'D in the type signature.
    type ClmSolverData =
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
