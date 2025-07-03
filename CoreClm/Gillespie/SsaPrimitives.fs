namespace Gillespie

module SsaPrimitives =

    /// Called MoleculeCount in Softellect.Math.Models
    type NoOfMolecules =
        | NoOfMolecules of int

        static member zero = NoOfMolecules 0
        static member one = NoOfMolecules 1

        static member (+) (NoOfMolecules a, NoOfMolecules b) =
            NoOfMolecules (a + b)

        static member (-) (NoOfMolecules a, NoOfMolecules b) =
            max (a - b) 0 |> NoOfMolecules

        member n.value = let (NoOfMolecules v) = n in v


    type EnantiomericExcess =
        | EnantiomericExcess of double

        member r.value = let (EnantiomericExcess v) = r in v

        static member (-) (EnantiomericExcess a, EnantiomericExcess b) = (a - b) |> EeDiff


    /// The value (ee1 - ee2) where both are EnantiomericExcess.
    and EeDiff =
        | EeDiff of double

        member r.value = let (EeDiff v) = r in v


    type ChiralMolecule =
        {
            left: int
            right : int
        }

        member m.ee = (double (m.left - m.right)) / (double (max 1 (m.left + m.right))) |> EnantiomericExcess
        member m.n = m.left + m.right |> NoOfMolecules
        static member range (NoOfMolecules n) = [ for i in 0..n -> { left = i; right = n - i } ]


    /// TODO kk:20220320 - Use a custom type until the decision is made whether or not to use type Substance.
    /// If Substance is to be used the replace this by a type alias.
    type Species =
        | Any of string
        | Food
        | Waste
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


    type ReactionRateInfo =
        {
            info : ReactionNormalizedInfo
            rate : ReactionRate
            description : string
        }
