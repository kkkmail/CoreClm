namespace Gillespie

module TauLeaping2 =

    /// Represents the count of a specific molecule species in a reaction.
    /// - index: Index of the molecule in the system.
    /// - count: Count of molecules of this species in the reaction.
    type MoleculeCount = 
        {
            index: int
            count: int
        }


    /// Represents the structure of a chemical reaction.
    /// - reactants: A list of molecule counts representing reactants in the reaction.
    /// - products: A list of molecule counts representing products of the reaction.
    /// - rateConstant: The rate constant of the reaction.
    type Reaction = 
        {
            reactants: MoleculeCount list
            products: MoleculeCount list
            rateConstant: float
        }


    /// Represents the state of the chemical system.
    /// - molecules: An array where each int value represents the count of molecules of a particular species in the system.
    /// - reactions: A list of reactions that can occur in the system.
    type SystemState = 
        {
            molecules: int array
            reactions: Reaction list
        }


    /// Calculates the propensity of a given reaction based on the current molecule counts.
    let getPropensity reaction (molecules : int[]) =
        match reaction.reactants with
        | [] -> reaction.rateConstant  // If there are no reactants, return just the rate constant
        | _ -> 
            reaction.reactants 
            |> List.map (fun r -> (float molecules.[r.index]) ** (float r.count))
            |> List.reduce (*)
            |> (*) reaction.rateConstant


    //let tauLeapingStep (state: SystemState) tau =
    //    // Calculate propensities for all reactions
    //    let propensities = state.reactions |> List.map (fun r -> getPropensity r state.molecules)
    
    //    // Calculate the number of times each reaction fires
    //    let numFirings = propensities |> List.map (fun a -> a * tau |> round |> int)

    //    // Update molecule counts
    //    let updatedMolecules = Array.copy state.molecules

    //    state.reactions |> List.iteri (fun i reaction ->
    //        reaction.reactants |> List.iter (fun r -> updatedMolecules.[r.index] <- updatedMolecules.[r.index] - r.count * numFirings.[i])
    //        reaction.products |> List.iter (fun p -> updatedMolecules.[p.index] <- updatedMolecules.[p.index] + p.count * numFirings.[i]))

    //    { molecules = updatedMolecules; reactions = state.reactions }

    let tauLeapingStep (state: SystemState) tau =
        // Calculate propensities for all reactions
        let propensities = state.reactions |> List.map (fun r -> getPropensity r state.molecules)
    
        // Calculate the number of times each reaction fires
        let numFirings = propensities |> List.map (fun a -> a * tau |> round |> int)

        // Update molecule counts
        let updatedMolecules = Array.copy state.molecules

        state.reactions |> List.iteri (fun i reaction ->
            reaction.reactants |> List.iter (fun r -> updatedMolecules.[r.index] <- updatedMolecules.[r.index] - r.count * numFirings.[i])
            reaction.products |> List.iter (fun p -> updatedMolecules.[p.index] <- updatedMolecules.[p.index] + p.count * numFirings.[i]))

        // Check and correct negative counts
        for i in 0 .. updatedMolecules.Length - 1 do
            if updatedMolecules.[i] < 0 then updatedMolecules.[i] <- 0

        { molecules = updatedMolecules; reactions = state.reactions }


    //let simulateTauLeaping (initialState: SystemState) tau numSteps =
    //    let rec simulate state n =
    //        if n = 0 then [state]
    //        else state :: (simulate (tauLeapingStep state tau) (n - 1))
    //    simulate initialState numSteps

    let simulateTauLeaping (initialState: SystemState) tau numSteps =
        let mutable results = [initialState]
        let mutable currentState = initialState
        for i in 1..numSteps do
            currentState <- tauLeapingStep currentState tau
            results <- currentState :: results
        List.rev results



