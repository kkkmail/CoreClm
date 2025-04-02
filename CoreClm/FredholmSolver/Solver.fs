namespace FredholmSolver

open FredholmSolver.Primitives
open Softellect.Sys.Primitives
open Softellect.Math.Models
open FredholmSolver.Common

module Solver =

    let getNamePrefix name = $"{name}__"


    type PoissonEvolutionParam =
        {
            noOfEpochs : NoOfEpochs
            noOfCharts : int option
            maxChartPoints : int
            noOfFrames : int option
            duration : int // Clip duration in seconds.
            name : string
            outputFolder : FolderName
            dataFolder : FolderName
            odePackChartSupportFolder : FolderName
        }


    /// That's 'I in the type signature.
    type PoissonInitialData =
        {
            intModelParams : EeInfIntModelParams
            evolutionParam : PoissonEvolutionParam
        }

        member p.fullName = p.evolutionParam.name
        static member defaultValue mp n name =
            {
                intModelParams = mp
                evolutionParam =
                    {
                        noOfEpochs = n
                        noOfCharts = Some 20
                        maxChartPoints = 100_000
                        noOfFrames = Some 2_000
                        duration = 50
                        name = name
                        outputFolder = FolderName @"C:\EeInf"
                        dataFolder = FolderName @"Data" //FolderName @"C:\EeInf\Data"
                        odePackChartSupportFolder = FolderName @"C:\\GitHub\\CoreClm\\Math\\odePackChartSupport.m" // Need \\ for Wolfram.
                    }
            }

        member p.modelString =
            let a = p.intModelParams.modelString
            let b = p.evolutionParam.noOfEpochs.value |> int64 |> toModelStringInt64 0L |> Option.defaultValue EmptyString
            $"{a}_{b}"


    type PoissonParam =
        {
            runQueueId : RunQueueId
            initialData : PoissonInitialData
        }

        static member defaultValue mp n name =
            {
                runQueueId = RunQueueId.getNewId()
                initialData = PoissonInitialData.defaultValue mp n name
            }

        member p.modelString = p.initialData.modelString
        member p.fullName = p.initialData.fullName


    /// That's 'P in the type signature.
    type PoissonProgressData =
        {
            x : int
        }

        static member defaultValue =
            {
                x = 0
            }
