namespace FredholmSolver

open FredholmSolver.Primitives
open Softellect.Sys.Primitives
open Softellect.Math.Models
open FredholmSolver.Common

module Solver =

    let getNamePrefix name = $"{name}__"


    /// A collection of parameters needed for Poisson-based evolution.
    type PoissonEvolutionParam =
        {
            /// Number of evolution epochs.
            noOfEpochs : NoOfEpochs

            /// Number of charts to capture during the evolution of the system.
            noOfCharts : int option

            /// Maximum number of points in time-dependent charts.
            maxChartPoints : int

            /// Number of frames in the output animation.
            noOfFrames : int option

            /// Clip duration in seconds.
            duration : int

            /// Human-readable model name for convenience.
            name : string

            /// Folder where to put intermediate "data" files.
            dataFolder : FolderName

            /// Folder where to put the final "output" files.
            outputFolder : FolderName

            /// Location of support folder, which contains the necessary ".m" file(s).
            odePackChartSupportFolder : FolderName
        }

        static member defaultValue n name =
            {
                noOfEpochs = n
                noOfCharts = Some 20
                maxChartPoints = 100_000
                noOfFrames = Some 200
                duration = 50
                name = name
                dataFolder = FolderName "Data"
                outputFolder = FolderName "Output"
                odePackChartSupportFolder = FolderName @"C:\\GitHub\\CoreClm\\Math\\odePackChartSupport.m" // Need \\ for Wolfram.
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
                evolutionParam = PoissonEvolutionParam.defaultValue n name
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
