namespace Gillespie

//open Clm.Distributions
//open Clm.ReactionTypes
//open Clm.Substances

open SsaPrimitives

module SsaSolver =

    type SubstanceMap = Map<Species, NoOfMolecules>


    let createSubstanceMap d : SubstanceMap = d |> Map.ofList
    let getValueOrDefault m e d = m |> Map.tryFind e |> Option.defaultValue d


    let getPropensity (i: ReactionRateInfo) (m : SubstanceMap) : double =
        match i.info.inputNormalized with
        | [] -> i.rate.value
        | _ ->
            i.info.inputNormalized
            |> List.map (fun e -> m |> Map.tryFind e |> Option.defaultValue NoOfMolecules.zero)
            |> List.map (fun e -> double e.value)
            |> List.fold (fun acc r -> acc * r) i.rate.value


    let evolveSubstances m r =
        let remove map e =
            match map |> Map.tryFind e with
            | Some v -> map |> Map.add e (v - (NoOfMolecules 1))
            | None -> failwith $"No substances of type {e} are left. Cannot remove."

        let add map e =
            match map |> Map.tryFind e with
            | Some v -> map |> Map.add e (v + (NoOfMolecules 1))
            | None -> map |> Map.add e (NoOfMolecules 1)

        let m1 = r.inputNormalized |> List.fold remove m
        let m2 = r.outputNormalized |> List.fold add m1
        m2


    /// See: http://web.math.princeton.edu/multiscale/elv05.pdf
    type StateVector =
        {
            state : SubstanceMap
            reactions: List<ReactionRateInfo>
            volume : double
            time : double
        }

        static member create species reactions =
            {
                state = createSubstanceMap species
                reactions = reactions
                volume = 1.0
                time = 0.0
            }

        member s.totalPropensity =
            let a0 =
                s.reactions
                |> List.map (fun e -> getPropensity e s.state)
                |> List.sum

            a0

        member s.getReactionRateInfo r2a0 =
            let rec inner sum lst =
                match lst with
                | [] -> failwith "This must not happen because: 0 <= r2 < 1."
                | h :: t ->
                    let newSum = sum + (getPropensity h s.state)

                    if newSum >= r2a0 then h
                    else inner newSum t

            inner 0.0 s.reactions

        member s.evolve g1 g2 =
            let r1 = g1() % 1.0
            let r2 = g2() % 1.0
            let a0 = s.totalPropensity

            if a0 > 0.0
            then
                let deltaT = - (log r1) / (a0 / s.volume)
                let r = s.getReactionRateInfo (r2 * a0)
//                printfn $"t = {s.time}, r1 = {r1}, r2 = {r2}, a0 = {a0}, r = {r.description}."

                { s with time = s.time + deltaT; state = evolveSubstances s.state r.info }
            else
                s
