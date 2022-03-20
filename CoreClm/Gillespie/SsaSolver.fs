namespace Gillespie

open Clm.Distributions
open Clm.ReactionTypes
open Clm.Substances

module SsaSolver =
    
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
            reactionInfo : ReactionNormalizedInfo
            reactionRate : ReactionRate
        }
        
        
    let getPropensity i m =
        match i.reactionInfo.inputNormalized with
        | [] -> 0.0
        | _ ->
            i.reactionInfo.inputNormalized
            |> List.map (fun e -> m |> Map.tryFind e |> Option.defaultValue NoOfMolecules.defaultValue)
            |> List.map (fun e -> double e.value)
            |> List.fold (fun acc r -> acc * r) i.reactionRate.value

    
    type SubstanceMap = Map<Substance, NoOfMolecules>
        

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
                    
                    if newSum >= r2a0 then h.reactionInfo
                    else inner newSum t 
            
            
            inner 0.0 s.reactions
        
        member s.evolve r1 r2 =
            let a0 = s.totalPropensity
            let deltaT = - (log r1) / (a0 / s.volume)
            let r = s.getReactionRateInfo (r2 * a0)
            
            { s with time = s.time + deltaT; state = evolveSubstances s.state r } 

