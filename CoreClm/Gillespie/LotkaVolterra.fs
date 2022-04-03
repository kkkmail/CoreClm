namespace Gillespie

open SsaPrimitives

module LotkaVolterra =

    let fox = Any "fox"
    let hare = Any "hare"


    type LotkaVolterraRateData =
        {
            haresMultiplyRate : double // Hares multiply: dx / dt = a * x
            haresEatenRate : double // Hares got eaten: dx / dt = -b * x * y
            foxesDieOffRate : double // Foxes die off: dy / dt = -c * x
            foxesMultiplyRate : double // Foxes multiply: dy / dt = d * x * y
            haresResurrectRate : double // Hares resurrect: dx / dt = e
            foxesResurrectRate : double // Foxes resurrect: dx / dt = f
            haresDieOffRate : double // Hares die from overpopulation: dx / dt = -g * x * x
        }


    type LotkaVolterraInitData =
        {
            noOfHares : int
            noOfFoxes : int
        }


    type LotkaVolterraData =
        {
            t : double
            foxes: int
            hares : int
            step: int
        }


    // https://en.wikipedia.org/wiki/Lotka%E2%80%93Volterra_equations
    let createReactions d =
        [
            {
                description = "Hares multiply: dx / dt = a * x"
                info =
                    {
                        input = [ hare, 1 ]
                        output = [ hare, 2 ]
                    }
                    |> normalized
                rate = ReactionRate d.haresMultiplyRate
            }

            {
                description = "Hares got eaten: dx / dt = -b * x * y"
                info =
                    {
                        input = [ (hare, 1); (fox, 1) ]
                        output = [ (fox, 1) ]
                    }
                    |> normalized
                rate = ReactionRate d.haresEatenRate
            }

            {
                description = "Hares resurrect: dx / dt = e"
                info =
                    {
                        input = []
                        output = [ hare, 1 ]
                    }
                    |> normalized
                rate = ReactionRate d.haresResurrectRate
            }

            {
                description = "Hares die from overpopulation: dx / dt = -g * x * x"
                info =
                    {
                        input = [ hare, 2 ]
                        output = [ hare, 1 ]
                    }
                    |> normalized
                rate = ReactionRate d.haresDieOffRate
            }

            {
                description = "Foxes multiply: dy / dt = d * x * y"
                info =
                    {
                        input = [ (fox, 1); (hare, 1) ]
                        output = [ (fox, 2); (hare, 1) ]
                    }
                    |> normalized
                rate = ReactionRate d.foxesMultiplyRate
            }

            {
                description = "Foxes die off: dy / dt = -c * x"
                info =
                    {
                        input = [ fox, 1 ]
                        output = []
                    }
                    |> normalized
                rate = ReactionRate d.foxesDieOffRate
            }

            {
                description = "Foxes resurrect: dx / dt = f"
                info =
                    {
                        input = []
                        output = [ fox, 1 ]
                    }
                    |> normalized
                rate = ReactionRate d.foxesResurrectRate
            }
        ]


    let createSpecies d =
        [
            fox, NoOfMolecules d.noOfFoxes
            hare, NoOfMolecules d.noOfHares
        ]
