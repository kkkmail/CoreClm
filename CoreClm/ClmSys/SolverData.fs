namespace ClmSys

module SolverData =

    type EeData =
        {
            maxEe : double
            maxAverageEe : double
            maxWeightedAverageAbsEe : double
            maxLastEe : double
        }

        static member defaultValue =
            {
                maxEe = 0.0
                maxAverageEe = 0.0
                maxWeightedAverageAbsEe = 0.0
                maxLastEe = 0.0
            }
