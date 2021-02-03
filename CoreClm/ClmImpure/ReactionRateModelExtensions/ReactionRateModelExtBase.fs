namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRates
open ClmImpure.ReactionRateModelsAll

module ReactionRateModelExtBase =

    let tryGetModel getter (p : list<ReactionRateModelWithUsage>) = p |> List.tryPick getter


    let tryCreateModel picker creator (p, m) =
        match tryPickParam picker p with
        | Some (u, d), q ->
            let models =
                {
                    model = creator d
                    usage = u
                } :: m
            (q, models)
        | None, _ -> (p, m)


    let tryCreateModelWithBase picker creator baseGetter baseCreator (p, m) =
        let create b d u =
            {
                model = creator b d
                usage = u
            }

        match tryPickParam picker p with
        | Some (u, d), q ->
            match tryGetModel baseGetter m with
            | Some b -> q, (create b d u) :: m
            | None ->
                let (q1, m1) = baseCreator (q, m)

                match tryGetModel baseGetter m1 with
                | Some b -> q1, (create b d u) :: m1
                | None -> (q1, m1)
        | None, _ -> (p, m)


    let tryCreateAcModelWithBase x y z (p, m) =
        let (picker, creator) = x
        let (baseGetter, baseCreator) = y
        let (acGetter, acCreator) = z

        let create b d ac u =
            {
                model = creator b d ac
                usage = u
            }

        match tryPickParam picker p with
        | Some (u, d), q ->
            match tryGetModel baseGetter m, tryGetModel acGetter m with
            | Some b, Some ac -> q, (create b d ac u) :: m
            | Some b, None ->
                let (q2, m2) = acCreator (q, m)

                match tryGetModel acGetter m2 with
                | Some ac -> q2, (create b d ac u) :: m2
                | None -> (q2, m2)
            | None, Some ac ->
                let (q1, m1) = baseCreator (q, m)

                match tryGetModel baseGetter m1 with
                | Some b -> q1, (create b d ac u) :: m1
                | None -> (q1, m1)
            | None, None ->
                let (q1, m1) = baseCreator (q, m)
                let (q2, m2) = acCreator (q1, m1)

                match tryGetModel baseGetter m2, tryGetModel acGetter m2 with
                | Some b, Some ac -> q2, (create b d ac u) :: m2
                | _ -> (q2, m2)
        | None, _ -> (p, m)
