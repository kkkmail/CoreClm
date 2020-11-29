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
