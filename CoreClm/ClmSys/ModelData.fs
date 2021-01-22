namespace ClmSys

open ClmSys.DistributionData

module ModelData =

    type PairCollisionResolutionType =
        {
            collisionA : CollisionResolutionType
            collisionB : CollisionResolutionType
        }

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

        static member defaultValue = PairCollisionResolutionType.defaultValue |> EachInPair
        static member excludeDuplicateCatalysts = PairCollisionResolutionType.excludeDuplicateCatalysts |> EachInPair


    type TripleCollisionResolutionType =
        {
            collisionA : CollisionResolutionType
            collisionB : CollisionResolutionType
            collisionC : CollisionResolutionType
        }

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
