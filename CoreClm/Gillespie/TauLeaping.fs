namespace Gillespie

module TauLeaping =

    /// Represents the structure of a chemical reaction, including reactants, products, and the rate constant.
    /// - reactants: An array where each int value represents the count of molecules of a particular species participating as a reactant in the reaction.
    /// - products: An array where each int value represents the count of molecules of a particular species produced by the reaction.
    /// - rateConstant: The rate constant of the reaction.
    type Reaction = 
        {
            reactants: int array
            products: int array
            rateConstant: float
        }

    /// Represents the state of the chemical system, including the current count of molecules and the list of reactions.
    /// - molecules: An array where each int value represents the count of molecules of a particular species in the system.
    /// - reactions: A list of reactions that can occur in the system.
    type SystemState = 
        {
            molecules: int array
            reactions: Reaction list
        }

    /// Calculates the propensity of a given reaction based on the current molecule counts.
    /// The propensity is a measure of how likely a reaction is to occur.
    let getPropensity reaction (molecules : int[]) =
        reaction.reactants 
        |> Array.mapi (fun i count -> (float molecules.[i]) ** (float count))
        |> Array.reduce (*)
        |> (*) reaction.rateConstant

    /// Performs one tau-leaping step based on the current state of the system and a given time step (tau).
    /// Tau-leaping is a method to advance the state of a stochastic system by approximating the number of reaction firings in a fixed time step.
    let tauLeapingStep (state: SystemState) tau =
        // Calculate propensities for all reactions
        let propensities = state.reactions |> List.map (fun r -> getPropensity r state.molecules)
    
        // Calculate the number of times each reaction fires
        let numFirings = propensities |> List.map (fun a -> a * tau |> round |> int)

        // Update molecule counts
        let updatedMolecules = 
            state.molecules
            |> Array.mapi (fun j moleculeCount ->
                state.reactions
                |> List.mapi (fun i reaction -> 
                    moleculeCount - reaction.reactants.[j] * numFirings.[i] + reaction.products.[j] * numFirings.[i])
                |> List.sum)

        { molecules = updatedMolecules; reactions = state.reactions }

    /// Simulates the system using the tau-leaping method for a given number of steps.
    /// Returns a list of system states at each step of the simulation.
    let simulateTauLeaping (initialState: SystemState) tau numSteps =
        let rec simulate state n =
            if n = 0 then [state]
            else state :: (simulate (tauLeapingStep state tau) (n - 1))
        simulate initialState numSteps
