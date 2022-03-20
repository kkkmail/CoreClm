namespace Gillespie

//open Clm.Distributions
//open Clm.ReactionTypes
//open Clm.Substances

module SsaSolver =
    
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
        
        static member defaultValue = NoOfMolecules 0
        
        static member (+) (NoOfMolecules a, NoOfMolecules b) =
            NoOfMolecules (a + b)
        
        static member (-) (NoOfMolecules a, NoOfMolecules b) =
            max (a - b) 0 |> NoOfMolecules
            
        member n.value = let (NoOfMolecules v) = n in v         
            

    type ReactionRateInfo =
        {
            info : ReactionNormalizedInfo
            rate : ReactionRate
        }
        
        
    let getPropensity i m =
        match i.info.inputNormalized with
        | [] -> 0.0
        | _ ->
            i.info.inputNormalized
            |> List.map (fun e -> m |> Map.tryFind e |> Option.defaultValue NoOfMolecules.defaultValue)
            |> List.map (fun e -> double e.value)
            |> List.fold (fun acc r -> acc * r) i.rate.value

    
    type SubstanceMap = Map<Species, NoOfMolecules>
        

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
                    
                    if newSum >= r2a0 then h.info
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
                
                { s with time = s.time + deltaT; state = evolveSubstances s.state r }
            else
                s

