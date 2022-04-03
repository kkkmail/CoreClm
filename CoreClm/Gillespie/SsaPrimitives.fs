namespace Gillespie

module SsaPrimitives =

    type ChiralMolecule =
        {
            left: int
            right : int
        }

        member m.ee = (double (m.left - m.right)) / (double (max 1 (m.left + m.right)))
        member m.n = m.left + m.right


    /// TODO kk:20220320 - Use a custom type until the decision is made whether or not to use type Substance.
    /// If Substance is to be used the replace this by a type alias.
    type Species =
        | Any of string
        | Chiral of ChiralMolecule

//        member m.value = let (Molecule v) = m in v


    /// TODO kk:20220320 - Use a local type until the decision is made whether or not to use type Substance.
    type ReactionRate =
        | ReactionRate of double

        member r.value = let (ReactionRate v) = r in v


    /// TODO kk:20220320 - Use a local type until the decision is made whether or not to use type Substance.
    type ReactionNormalizedInfo =
        {
            inputNormalized : list<Species>
            outputNormalized : list<Species>
        }


    /// TODO kk:20220320 - Use a local type until the decision is made whether or not to use type Substance.
    type ReactionInfo =
        {
            input : list<Species * int>
            output : list<Species * int>
        }

    let normalized i =
        let normalize d =
            d
            |> List.map (fun (s, i) -> [ for _ in 0..(i-1) -> s ])
            |> List.concat
            |> List.sort

        {
            inputNormalized = i.input |> normalize
            outputNormalized = i.output |> normalize
        }


    type NoOfMolecules =
        | NoOfMolecules of int

        static member zero = NoOfMolecules 0
        static member one = NoOfMolecules 1

        static member (+) (NoOfMolecules a, NoOfMolecules b) =
            NoOfMolecules (a + b)

        static member (-) (NoOfMolecules a, NoOfMolecules b) =
            max (a - b) 0 |> NoOfMolecules

        member n.value = let (NoOfMolecules v) = n in v


    type ReactionRateInfo =
        {
            info : ReactionNormalizedInfo
            rate : ReactionRate
            description : string
        }
