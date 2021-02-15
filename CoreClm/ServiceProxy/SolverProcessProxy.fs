namespace ServiceProxy

open ClmSys.GeneralPrimitives

module SolverProcessProxy =

    type SolverProcessProxy =
        {
            dummy : int
        }


        static member create (RunQueueId q) : SolverProcessProxy =
            failwith "SolverProcessProxy.create is not yet implemented."

