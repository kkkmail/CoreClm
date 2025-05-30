﻿namespace Clm

open System
open FSharp.Collections
open Primitives.VersionInfo
//open Primitives.GeneralData
open ClmSys.ContGenPrimitives
//open ClmSys.WorkerNodePrimitives
open Clm.Substances
open Clm.ReactionTypes
open ClmSys.ModelData
open Clm.ReactionRates
open ClmSys.SolverRunnerPrimitives
//open Primitives.GeneralPrimitives
//open Primitives.GeneralData
open Softellect.Sys.Primitives
open Softellect.DistributedProcessing.Primitives.Common

module ModelParams =

    [<Literal>]
    let DefaultRootFolder = DefaultRootDrive + @":\" + ContGenBaseName + @"\Clm\"

    [<Literal>]
    let DefaultResultLocationFolder = DefaultRootFolder + "Results"

    [<Literal>]
    let DefaultFileStorageFolder = DefaultRootFolder + "FileStorage"

    [<Literal>]
    let ModelDataFolder = __SOURCE_DIRECTORY__ + @"\..\Model\"

    [<Literal>]
    let DefaultModelDataFile = "ModelCode"


    let toModelName (n : Guid) = n.ToString()


    type ModelInfo =
        {
            versionNumber : string
            modelDataId : ModelDataId
            numberOfSubstances : int
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength
            seedValue : int
            clmDefaultValueId : ClmDefaultValueId
            description : string option
        }

    type ModelDataParams =
        {
            modelInfo : ModelInfo
            allParams : array<ReactionRateModelParamWithUsage>
            collisionData : CollisionData
            dictionaryUpdateType : DictionaryUpdateType
        }


    type AllSubstData =
        {
            allSubst : list<Substance>
            allInd : Map<Substance, int>
            allRawReactions : list<ReactionName * int64>
            allReactions : list<ReactionName * int64>
        }


    type BinaryInfo =
        {
            aminoAcids : list<AminoAcid>
            maxPeptideLength : MaxPeptideLength // Cannot be easily inferred from the binary data but is needed here and there.
            allSubstData : AllSubstData
        }

        member info.getTotals x =
            getTotalsValue info.allSubstData.allInd info.allSubstData.allSubst info.aminoAcids x

        member info.getTotalSubst x =
            getTotalSubstValue info.allSubstData.allInd info.allSubstData.allSubst x


    type ModelDataRegularParams =
        {
            modelDataParams : ModelDataParams
            allSubstData : AllSubstData
        }


    type ModelDataFuncParams =
        {
            getTotals : array<double> -> array<double * double>
            getTotalSubst : array<double> -> double
            getDerivative : array<double> -> array<double>
        }


    type ModelDataParamsWithExtraData =
        {
            regularParams : ModelDataRegularParams
            funcParams : ModelDataFuncParams
        }

        member info.binaryInfo =
            {
                aminoAcids = AminoAcid.getAminoAcids info.regularParams.modelDataParams.modelInfo.numberOfAminoAcids
                maxPeptideLength = info.regularParams.modelDataParams.modelInfo.maxPeptideLength
                allSubstData = info.regularParams.allSubstData
            }


    type ClmDefaultValue =
        {
            clmDefaultValueId : ClmDefaultValueId
            defaultRateParams : ReactionRateProviderParams
            description : string option
        }


    /// Updates description with a given one if there none available at the source.
    let private tryUpdateDescription d e =
        match e.description with
        | Some _ -> e
        | None -> { e with description = Some d }


    let updateDescription d lst = lst |> List.map (tryUpdateDescription d)


    /// Additional information needed to produce command line params for solver runner.
    type ModelCommandLineData =
        {
            modelDataId : ModelDataId
            workerNodeId : WorkerNodeId
            minUsefulEe : MinUsefulEe
            remote : bool
            noOfProgressPoints : int
        }


    /// Parameters, which come from ClmTask & related data.
    type ModelCommandLineParam =
        {
            tEnd : decimal
            y0 : decimal
            useAbundant : bool
        }


    type ResultInfo =
        {
            resultLocation : string
            separator : string
        }

        static member defaultValue =
            {
                resultLocation = DefaultResultLocationFolder
                separator = "_"
            }


    type ClmTaskDetails =
        {
            clmDefaultValueId : ClmDefaultValueId
            clmTaskPriority : ClmTaskPriority
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength
        }


    type ClmTaskInfo =
        {
            clmTaskId : ClmTaskId
            taskDetails : ClmTaskDetails
        }


    type ClmTask =
        {
            clmTaskInfo : ClmTaskInfo
            commandLineParams : list<ModelCommandLineParam>
            numberOfRepetitions : int
            remainingRepetitions : int
            createdOn : DateTime
        }
