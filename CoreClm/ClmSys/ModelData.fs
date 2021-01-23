namespace ClmSys

open ClmSys.DistributionData

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
            let tryCreate v = p |> Map.tryFind v |> Option.map CollisionResolutionType.tryCreate |> Option.flatten

            match nameof(d.collisionA) |> tryCreate, nameof(d.collisionB) |> tryCreate with
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
            | nameof(PairCollision) -> Some PairCollision
            | _ ->
                let start = $"{nameof(EachInPair)}{DiscriminatedUnionSeparator}"

                match s.Replace(" ", "").StartsWith(start) with
                | true -> s.Substring(start.Length) |> PairCollisionResolutionType.tryDeserialize |> Option.bind (fun e -> e |> EachInPair |> Some)
                | false -> None

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
            let tryCreate v = p |> Map.tryFind v |> Option.map CollisionResolutionType.tryCreate |> Option.flatten

            match nameof(d.collisionA) |> tryCreate, nameof(d.collisionB) |> tryCreate, nameof(d.collisionC) |> tryCreate with
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

        static member tryDeserialize (s : string) =
            match s with
            | nameof(TripleCollision) -> Some TripleCollision
            | _ ->
                let start = $"{nameof(EachInTriple)}{DiscriminatedUnionSeparator}"

                match s.Replace(" ", "").StartsWith(start) with
                | true -> s.Substring(start.Length) |> TripleCollisionResolutionType.tryDeserialize |> Option.bind (fun e -> e |> EachInTriple |> Some)
                | false -> None

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
