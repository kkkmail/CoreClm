namespace Gillespie

open Gillespie.SsaPrimitives

module ProtoCells =

    type RateMultiplierData =
        {
            x0 : double
            a0 : double
        }

        ///These numbers approximate binomial rate multiplier for m = 20, n = 10^4 with very good precision.
        /// See: Freedholm__002.nb
        static member m20_n10000 =
            {
                x0 = 1.025
                a0 = 0.289021
            }

        ///These numbers approximate binomial rate multiplier for m = 4, n = 10^4 with good precision.
        /// See: Freedholm__002.nb
        static member m4_n10000 =
            {
                x0 = 1.005
                a0 = 0.552177
            }


    /// Calculates approximate normalized rate multiplier.
    let normalizedRateMultiplier d x = 1.0 + d.a0 * (d.x0 - sqrt(d.x0 * d.x0 - x * x))


    type ProtoCell =
        | ProtoCell of ChiralMolecule

        member c.value = let (ProtoCell v) = c in v


    type ProtoCellData =
        {
            noOfMolecules : NoOfMolecules
            kFunc : NoOfMolecules -> EnantiomericExcess -> double
            kFuncEps : double // kFunc values less than that will be considered as exact zeros.
            pFunc : NoOfMolecules -> EeDiff -> double
            pFuncEps : double // kFunc values less than that will be considered as exact zeros.
            asymmetryFactor : double // g Factor
        }


    type ProtoCellReactionType =
        | DecayIntoWaste
//        | DecayIntoSmthElse // Not Implemented yet
//        | Grow // Not Implemented yet
//        | Divide // Not Implemented yet
        | Multiply // Consumes food and energy and produces another protocell.
