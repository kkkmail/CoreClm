namespace Gillespie

open Gillespie.SsaPrimitives

module ProtoCells =


    type ProtoCell =
        | ProtoCell of ChiralMolecule

        member c.value = let (ProtoCell v) = c in v


    type ProtoCellReactionType =
        | DecayIntoWaste
//        | DecayIntoSmthElse // Not Implemented yet
//        | Grow // Not Implemented yet
//        | Divide // Not Implemented yet
        | Multiply // Consumes food and energy and produces another protocell.
