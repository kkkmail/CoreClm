namespace ClmSys

open ClmSys.DistributionData
open Softellect.Sys

module ModelData =

    // ========================================== //
    // Move to Softellect.Sys.AppSettings

    [<Literal>]
    let ValueSeparator = ":"


    [<Literal>]
    let ListSeparator = ","


    [<Literal>]
    let DiscriminatedUnionSeparator = "|"


    /// Expects a string in the form:
    ///     someField1:SomeValue1,someField2:SomeValue2
    let parseSimpleSetting (s : string) =
        let p =
            s.Split ListSeparator
            |> List.ofArray
            |> List.map (fun e -> e.Split ValueSeparator)
            |> List.map (fun e -> e |> Array.map (fun a -> a.Trim()))
            |> List.map (fun e -> if e.Length = 2 then Some (e.[0], e.[1]) else None)
            |> List.choose id
            |> Map.ofList

        p

    // ========================================== //


    /// Some of the dictionaries (e.g. [en/ac] ligation catalytic related ones) may become extremely large.
    /// Subsequently for such dictionaries we may want to store only non-optional data in the dictionary.
    type DictionaryUpdateType =
        /// Store all data in the dictionary. The size of the dictionary might become very large.
        | AllRateData

        /// Store only non-optional data in the dictionary.
        /// Do not use this for simple reactions where stored optional data
        /// is actually used to show that the reaction does NOT have a rate.
        /// A projection function Reaction -> Key (which is usually a catalyst)
        /// is used to project a reaction into a smaller [space] representation.
        /// This is currently used by [en/ac] catalytic ligation reactions where the numbers
        /// are just too big.
        | NonOptionalRateDataOnly


        member u.serialize() = $"{u}"

        static member tryDeserialize (s : string) =
            match s with
            | nameof(AllRateData) -> Ok AllRateData
            | nameof(NonOptionalRateDataOnly) -> Ok NonOptionalRateDataOnly
            | _ -> Error s


    type PairCollisionResolutionType =
        {
            collisionA : CollisionResolutionType
            collisionB : CollisionResolutionType
        }

        member collision.serialize() =
            $"{nameof(collision.collisionA)}{ValueSeparator}{collision.collisionA}{ListSeparator}{nameof(collision.collisionB)}{ValueSeparator}{collision.collisionB}"

        static member tryDeserialize (s : string) =
            let d = PairCollisionResolutionType.defaultValue
            let p = parseSimpleSetting s
            let tryDeserialize v = p |> Map.tryFind v |> Option.map CollisionResolutionType.tryDeserialize |> Option.flatten

            match nameof(d.collisionA) |> tryDeserialize, nameof(d.collisionB) |> tryDeserialize with
            | Some a, Some b ->
                {
                    collisionA = a
                    collisionB = b
                }
                |> Some
            | _ -> None


        static member defaultValue =
            {
                collisionA = NoCollisionResolution
                collisionB = NoCollisionResolution
            }

        static member excludeDuplicateCatalysts =
            {
                collisionA = NoCollisionResolution
                collisionB = ExcludeDuplicates
            }


    type PairCollisionResolution =
        | PairCollision
        | EachInPair of PairCollisionResolutionType

        member p.serialize() =
            match p with
            | PairCollision -> nameof(PairCollision)
            | EachInPair e -> $"{nameof(EachInPair)}{DiscriminatedUnionSeparator}{e.serialize()}"


        static member tryDeserialize (s : string) =
            match s with
            | nameof(PairCollision) -> Ok PairCollision
            | _ ->
                let start = $"{nameof(EachInPair)}{DiscriminatedUnionSeparator}"

                match s.Replace(" ", "").StartsWith(start) with
                | true ->
                    let r = s.Substring(start.Length) |> PairCollisionResolutionType.tryDeserialize |> Option.bind (fun e -> e |> EachInPair |> Some)

                    match r with
                    | Some v -> Ok v
                    | None -> Error s
                | false -> Error s

        static member defaultValue = PairCollisionResolutionType.defaultValue |> EachInPair
        static member excludeDuplicateCatalysts = PairCollisionResolutionType.excludeDuplicateCatalysts |> EachInPair


    type TripleCollisionResolutionType =
        {
            collisionA : CollisionResolutionType
            collisionB : CollisionResolutionType
            collisionC : CollisionResolutionType
        }

        member collision.serialize() =
            $"{nameof(collision.collisionA)}{ValueSeparator}{collision.collisionA}{ListSeparator}{nameof(collision.collisionB)}{ValueSeparator}{collision.collisionB}{ListSeparator}{nameof(collision.collisionC)}{ValueSeparator}{collision.collisionC}"

        static member tryDeserialize (s : string) =
            let d = TripleCollisionResolutionType.defaultValue
            let p = parseSimpleSetting s
            let tryDeserialize v = p |> Map.tryFind v |> Option.map CollisionResolutionType.tryDeserialize |> Option.flatten

            match nameof(d.collisionA) |> tryDeserialize, nameof(d.collisionB) |> tryDeserialize, nameof(d.collisionC) |> tryDeserialize with
            | Some a, Some b, Some c ->
                {
                    collisionA = a
                    collisionB = b
                    collisionC = c
                }
                |> Some
            | _ -> None


        static member defaultValue =
            {
                collisionA = NoCollisionResolution
                collisionB = NoCollisionResolution
                collisionC = NoCollisionResolution
            }

        static member excludeDuplicateCatalysts =
            {
                collisionA = NoCollisionResolution
                collisionB = ExcludeDuplicates
                collisionC = NoCollisionResolution
            }


    type TripleCollisionResolution =
        | TripleCollision
        | EachInTriple of TripleCollisionResolutionType

        member p.serialize() =
            match p with
            | TripleCollision -> nameof(TripleCollision)
            | EachInTriple e -> $"{nameof(EachInTriple)}{DiscriminatedUnionSeparator}{e.serialize()}"

        static member tryDeserialize (s : string) : Result<TripleCollisionResolution, string> =
            match s with
            | nameof(TripleCollision) -> Ok TripleCollision
            | _ ->
                let start = $"{nameof(EachInTriple)}{DiscriminatedUnionSeparator}"

                match s.Replace(" ", "").StartsWith(start) with
                | true ->
                    let r = s.Substring(start.Length) |> TripleCollisionResolutionType.tryDeserialize |> Option.bind (fun e -> e |> EachInTriple |> Some)

                    match r with
                    | Some v -> Ok v
                    | None -> Error s
                | false -> Error s

        static member defaultValue = TripleCollisionResolutionType.defaultValue |> EachInTriple
        static member excludeDuplicateCatalysts = TripleCollisionResolutionType.excludeDuplicateCatalysts |> EachInTriple


    type CollisionData =
        {
            sugSynthColl : PairCollisionResolution
            catSynthColl : PairCollisionResolution
            enCatSynthColl : TripleCollisionResolution
            acCatSynthColl : PairCollisionResolution
            catDestrColl : PairCollisionResolution
            enCatDestrColl : TripleCollisionResolution
            acCatDestrColl : PairCollisionResolution
            catLigColl : PairCollisionResolution
            enCatLigColl : TripleCollisionResolution
            acFwdCatLigColl : PairCollisionResolution
            acBkwCatLigColl : PairCollisionResolution
            catRacemColl : PairCollisionResolution
            enCatRacemColl : TripleCollisionResolution
            acCatRacemColl : PairCollisionResolution
            sedDirColl : PairCollisionResolution
            acColl : PairCollisionResolution
        }

        static member defaultValue =
            {
                sugSynthColl = PairCollisionResolution.defaultValue
                catSynthColl = PairCollisionResolution.defaultValue
                enCatSynthColl = TripleCollisionResolution.defaultValue
                acCatSynthColl = PairCollisionResolution.defaultValue
                catDestrColl = PairCollisionResolution.defaultValue
                enCatDestrColl = TripleCollisionResolution.defaultValue
                acCatDestrColl = PairCollisionResolution.defaultValue
                catLigColl = PairCollisionResolution.defaultValue
                enCatLigColl = TripleCollisionResolution.defaultValue
                acFwdCatLigColl = PairCollisionResolution.defaultValue
                acBkwCatLigColl = PairCollisionResolution.defaultValue
                catRacemColl = PairCollisionResolution.defaultValue
                enCatRacemColl = TripleCollisionResolution.defaultValue
                acCatRacemColl = PairCollisionResolution.defaultValue
                sedDirColl = PairCollisionResolution.defaultValue
                acColl = PairCollisionResolution.defaultValue
            }

        static member excludeDuplicateCatalysts =
            {
                sugSynthColl = PairCollisionResolution.excludeDuplicateCatalysts
                catSynthColl = PairCollisionResolution.excludeDuplicateCatalysts
                enCatSynthColl = TripleCollisionResolution.excludeDuplicateCatalysts
                acCatSynthColl = PairCollisionResolution.excludeDuplicateCatalysts
                catDestrColl = PairCollisionResolution.excludeDuplicateCatalysts
                enCatDestrColl = TripleCollisionResolution.excludeDuplicateCatalysts
                acCatDestrColl = PairCollisionResolution.excludeDuplicateCatalysts
                catLigColl = PairCollisionResolution.excludeDuplicateCatalysts
                enCatLigColl = TripleCollisionResolution.excludeDuplicateCatalysts
                acFwdCatLigColl = PairCollisionResolution.excludeDuplicateCatalysts
                acBkwCatLigColl = PairCollisionResolution.excludeDuplicateCatalysts
                catRacemColl = PairCollisionResolution.excludeDuplicateCatalysts
                enCatRacemColl = TripleCollisionResolution.excludeDuplicateCatalysts
                acCatRacemColl = PairCollisionResolution.excludeDuplicateCatalysts
                sedDirColl = PairCollisionResolution.excludeDuplicateCatalysts
                acColl = PairCollisionResolution.excludeDuplicateCatalysts
            }
