﻿namespace Clm.Generator

open FSharp.Collections

open Primitives.VersionInfo
open Clm.Substances
open Clm.Reactions
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRates
open Clm.ModelParams
open Clm.CalculationData
open ClmImpure.RateProvider
open ClmImpure.ReactionsExt
open ClmImpure.ReactionRateFunctions
open Clm.Generator.ReactionRatesExt
open Clm.Distributions
open ClmSys.ModelData

module ClmModelData =

    type UpdateFuncType =
        | UseArray
        | UseVariables
        | UseFunctions


    type ModelGenerationParams =
        {
            versionNumber : string
            taskDetails : ClmTaskDetails
            reactionRateModelParams : List<ReactionRateModelParam>
            updateFuncType : UpdateFuncType
            successNumberType : SuccessNumberType
            collisionData : CollisionData
            dictionaryUpdateType : DictionaryUpdateType
            seedValue : int option
            description : string option
        }


    type AllParams =
        {
            modelGenerationParams : ModelGenerationParams
            modelCommandLineParams : list<ModelCommandLineParam>
        }

        static member create u coll so g (c : ClmTask) =
            match g c.clmTaskInfo.taskDetails.clmDefaultValueId with
            | Ok v ->
                {
                    modelGenerationParams =
                        {
                            versionNumber = VersionNumberValue
                            taskDetails = c.clmTaskInfo.taskDetails
                            reactionRateModelParams = v.defaultRateParams.rateParams
                            updateFuncType = UseFunctions
                            successNumberType = v.defaultRateParams.successNumberType
                            collisionData = coll
                            dictionaryUpdateType = u
                            seedValue = so
                            description = v.description
                        }

                    modelCommandLineParams = c.commandLineParams
                }
                |> Ok
            | Error e -> Error e

    let reactionShift updateFuncType =
        match updateFuncType with
        | UseArray -> "    "
        | UseVariables -> "    "
        | UseFunctions -> ""


    let generateSubst() =
            @"
    let aminoAcids = AminoAcid.getAminoAcids numberOfAminoAcids
    let chiralAminoAcids = ChiralAminoAcid.getAminoAcids numberOfAminoAcids
    let peptides = Peptide.getPeptides maxPeptideLength numberOfAminoAcids
    let peptideCatalysts = getPeptideCatalysts peptides
    let allSubst = createAllSubst chiralAminoAcids peptides peptideCatalysts
    let allInd = createAllInd allSubst
"


    type RateGenerationCommonData =
        {
            substInfo : SubstInfo
            sugSynth : list<ChiralSugar * SugCatalyst>
            catSynthPairs : list<SynthesisReaction * SynthCatalyst>
            enCatSynth : list<SynthesisReaction * EnSynthCatalyst * ChiralSugar>
            acCatSynth : list<SynthesisReaction * AcSynthCatalyst>
            catDestrPairs : list<DestructionReaction * DestrCatalyst>
            enCatDestr : list<DestructionReaction * EnDestrCatalyst * ChiralSugar>
            acCatDestr : list<DestructionReaction * AcDestrCatalyst>
            catLigPairs : list<LigationReaction * LigCatalyst>
            enCatLig : list<LigationReaction * EnLigCatalyst * ChiralSugar>
            acFwdCatLig : list<LigationReaction * AcFwdLigCatalyst>
            acBkwCatLig : list<LigationReaction * AcBkwLigCatalyst>
            catRacemPairs : list<RacemizationReaction * RacemizationCatalyst>
            enCatRacem : list<RacemizationReaction * EnRacemCatalyst * ChiralSugar>
            acCatRacem : list<RacemizationReaction * AcRacemCatalyst>
            sedDirPairs : list<ChiralAminoAcid * SedDirAgent>
            acPairs : list<ChiralSugar * Peptide>
        }

        member data.getReactions rnd rateProvider t n =
            let createReactions c l =
                let create a = c a |> AnyReaction.tryCreateReaction rnd rateProvider t

                l
                |> List.map create
                |> List.choose id
                |> List.concat

            match n with
            | FoodCreationName -> [ AnyReaction.tryCreateReaction rnd rateProvider t (FoodCreationReaction |> FoodCreation) ] |> List.choose id |> List.concat
            | WasteRemovalName -> [ AnyReaction.tryCreateReaction rnd rateProvider t (WasteRemovalReaction |> WasteRemoval) ] |> List.choose id |> List.concat
            | WasteRecyclingName -> [ AnyReaction.tryCreateReaction rnd rateProvider t (WasteRecyclingReaction |> WasteRecycling) ] |> List.choose id |> List.concat
            | SynthesisName -> createReactions (fun a -> SynthesisReaction a |> Synthesis) data.substInfo.chiralAminoAcids
            | SugarSynthesisName -> createReactions (fun a -> SugarSynthesisReaction a |> SugarSynthesis) data.sugSynth
            | DestructionName -> createReactions (fun a -> DestructionReaction a |> Destruction) data.substInfo.chiralAminoAcids
            | CatalyticSynthesisName -> createReactions (fun x -> CatalyticSynthesisReaction x |> CatalyticSynthesis) data.catSynthPairs
            | EnCatalyticSynthesisName -> createReactions (fun x -> EnCatalyticSynthesisReaction x |> EnCatalyticSynthesis) data.enCatSynth
            | AcCatalyticSynthesisName -> createReactions (fun x -> AcCatalyticSynthesisReaction x |> AcCatalyticSynthesis) data.acCatSynth
            | CatalyticDestructionName -> createReactions (fun x -> CatalyticDestructionReaction x |> CatalyticDestruction) data.catDestrPairs
            | EnCatalyticDestructionName -> createReactions (fun x -> EnCatalyticDestructionReaction x |> EnCatalyticDestruction) data.enCatDestr
            | AcCatalyticDestructionName -> createReactions (fun x -> AcCatalyticDestructionReaction x |> AcCatalyticDestruction) data.acCatDestr
            | LigationName -> createReactions (fun x -> x |> Ligation) data.substInfo.ligationPairs
            | CatalyticLigationName -> createReactions (fun x -> CatalyticLigationReaction x |> CatalyticLigation) data.catLigPairs
            | EnCatalyticLigationName -> createReactions (fun x -> EnCatalyticLigationReaction x |> EnCatalyticLigation) data.enCatLig
            | AcFwdCatalyticLigationName -> createReactions (fun x -> AcFwdCatalyticLigationReaction x |> AcFwdCatalyticLigation) data.acFwdCatLig
            | AcBkwCatalyticLigationName -> createReactions (fun x -> AcBkwCatalyticLigationReaction x |> AcBkwCatalyticLigation) data.acBkwCatLig
            | SedimentationDirectName -> createReactions (fun (c, r) -> SedimentationDirectReaction ([ c ] |> SedDirReagent, r) |> SedimentationDirect) data.sedDirPairs
            | SedimentationAllName -> []
            | RacemizationName -> createReactions (fun a -> RacemizationReaction a |> Racemization) data.substInfo.chiralAminoAcids
            | CatalyticRacemizationName -> createReactions (fun x -> CatalyticRacemizationReaction x |> CatalyticRacemization) data.catRacemPairs
            | EnCatalyticRacemizationName -> createReactions (fun x -> EnCatalyticRacemizationReaction x |> EnCatalyticRacemization) data.enCatRacem
            | AcCatalyticRacemizationName -> createReactions (fun x -> AcCatalyticRacemizationReaction x |> AcCatalyticRacemization) data.acCatRacem
            | ActivationName -> createReactions (fun x -> ActivationReaction x |> Activation) data.acPairs


    let generatePairs<'A, 'B when 'A : equality and 'B : equality> (rnd : RandomValueGetter) (i : RateGeneratorInfo<'A, 'B>) (rateProvider : ReactionRateProvider) =
        // !!! must adjust for 4x reduction due to grouping of (A + B, A + E(B), E(A) + B, E(A) + E(B))
        let noOfTries = (int64 i.a.Length) * (int64 i.b.Length) / 4L
//        printfn $"\n\ngeneratePairs:\n    noOfTries = {noOfTries}\n    typedefof<'A> = {(typedefof<'A>)}\n    typedefof<'B> = {(typedefof<'B>)}\n    pairCollision = %0A{i.pairCollision}\n    successNumberType = %0A{i.successNumberType}"

        // PairCollision should ensure that when individual duplicates are allowed we still won't get duplicate pairs.
        // This is an extremely rare (and currently unused) scenario and as such implementation is not worth an effort.
        match i.pairCollision with
        | PairCollision -> invalidOp $"generatePairs: {nameof(i.pairCollision)} = {PairCollision} is not supported yet!"
        | EachInPair ct ->
            let sng =
                rnd
                |>
                match i.successNumberType with
                | RandomValueBased -> RandomValueGetterBased
                | ThresholdBased -> ThresholdValueBased

            match rateProvider.tryGetPrimaryDistribution i.reactionName with
            | Some d ->
                let generate data getEnantiomer coll (idx, gen) =
                    let (i, a) = generateValue d rnd data getEnantiomer coll idx
                    (i, a :: gen)

                let generateA a = generate i.a i.getEnantiomerA ct.collisionA a
                let generateB b = generate i.b i.getEnantiomerB ct.collisionB b

                let sn = d.successNumber sng noOfTries
                printfn $"generatePairs: reaction: {i.reactionName}, noOfTries = {noOfTries}, threshold = {d.thresholdValue}, successNumberType = {i.successNumberType}, successNumber = {sn}"

                let ((_, a), (_, b)) =
                    [ for _ in 1..sn -> ()]
                    |> List.fold (fun (a, b) _ -> (generateA a, generateB b)) (([], []), ([], []))

                let retVal = (a, b) ||> List.zip |> List.rev
//                retVal |> List.map (fun (a, b) -> printfn "    %s, %s" (a.ToString()) (b.ToString())) |> ignore
                retVal
            | None -> []


    let generateTriples<'A, 'B, 'C when 'A : equality and 'B : equality and 'C : equality> (rnd : RandomValueGetter) (i : RateGeneratorInfo<'A, 'B, 'C>) (rateProvider : ReactionRateProvider) =
        // ??? must adjust for 8X ??? reduction due to grouping ???
        let noOfTries = (int64 i.a.Length) * (int64 i.b.Length) * (int64 i.c.Length) / 8L
//        printfn $"\n\ngenerateTriples:\n    noOfTries = {noOfTries}\n    typedefof<'A> = {(typedefof<'A>)}\n    typedefof<'B> = {(typedefof<'B>)}\n    typedefof<'C> = {(typedefof<'C>)}\n    tripleCollision = %0A{i.tripleCollision}\n    successNumberType = %0A{i.successNumberType}"

        // TripleCollision should ensure that when individual duplicates are allowed we still won't get duplicate triples.
        // This is an extremely rare (and currently unused) scenario and as such implementation is not worth an effort.
        match i.tripleCollision with
        | TripleCollision -> invalidOp $"generateTriples: {nameof(i.tripleCollision)} = {TripleCollision} is not supported yet!"
        | EachInTriple ct ->
            let sng =
                rnd
                |>
                match i.successNumberType with
                | RandomValueBased -> RandomValueGetterBased
                | ThresholdBased -> ThresholdValueBased

            match rateProvider.tryGetPrimaryDistribution i.reactionName with
            | Some d ->
                let generate data getEnantiomer coll (idx, gen) =
                    let (i, a) = generateValue d rnd data getEnantiomer coll idx
                    (i, a :: gen)

                let generateA a = generate i.a i.getEnantiomerA ct.collisionA a
                let generateB b = generate i.b i.getEnantiomerB ct.collisionB b
                let generateC c = generate i.c i.getEnantiomerC ct.collisionC c

                let sn = d.successNumber sng noOfTries
                printfn $"generateTriples: reaction: {i.reactionName}, noOfTries = {noOfTries}, threshold = {d.thresholdValue}, successNumberType = {i.successNumberType}, successNumber = {sn}"

                let ((_, a), (_, b), (_, c)) =
                    [ for _ in 1..sn -> ()]
                    |> List.fold (fun (a, b, c) _ -> (generateA a, generateB b, generateC c)) (([], []), ([], []), ([], []))

                let retVal = (a, b, c) |||> List.zip3 |> List.rev
//                retVal |> List.map (fun (a, b, c) -> printfn "    %s, %s, %s" (a.ToString()) (b.ToString()) (c.ToString())) |> ignore
                retVal
            | None -> []


    type RandomChoiceModelData =
        {
            commonData : RateGenerationCommonData
            collisionData : CollisionData
        }

        member data.noOfRawReactions n =
            let si = data.commonData.substInfo
            let sugLen = int64 ChiralSugar.all.Length

            match n with
            | FoodCreationName -> 1L
            | WasteRemovalName -> 1L
            | WasteRecyclingName -> 1L
            | SynthesisName -> int64 si.chiralAminoAcids.Length
            | SugarSynthesisName -> sugLen * (int64 si.sugSynthCatalysts.Length)
            | DestructionName -> int64 si.chiralAminoAcids.Length
            | CatalyticSynthesisName -> (int64 si.synthesisReactions.Length) * (int64 si.synthCatalysts.Length)
            | EnCatalyticSynthesisName -> (int64 si.synthesisReactions.Length) * (int64 si.enSynthCatalysts.Length) * sugLen * 2L
            | AcCatalyticSynthesisName -> (int64 si.synthesisReactions.Length) * (int64 si.acSynthCatalysts.Length)
            | CatalyticDestructionName -> (int64 si.destructionReactions.Length) * (int64 si.destrCatalysts.Length)
            | EnCatalyticDestructionName -> (int64 si.destructionReactions.Length) * (int64 si.enDestrCatalysts.Length) * sugLen * 2L
            | AcCatalyticDestructionName -> (int64 si.destructionReactions.Length) * (int64 si.acDestrCatalysts.Length)
            | LigationName -> int64 si.ligationPairs.Length
            | CatalyticLigationName -> (int64 si.ligationReactions.Length) * (int64 si.ligCatalysts.Length)
            | EnCatalyticLigationName -> (int64 si.ligationReactions.Length) * (int64 si.enLigCatalysts.Length) * sugLen * 2L
            | AcFwdCatalyticLigationName -> (int64 si.ligationReactions.Length) * (int64 si.acFwdLigCatalysts.Length)
            | AcBkwCatalyticLigationName -> (int64 si.ligationReactions.Length) * (int64 si.acBkwLigCatalysts.Length)
            | SedimentationDirectName -> (int64 si.allChains.Length) * (int64 si.allChains.Length)
            | SedimentationAllName -> int64 si.chiralAminoAcids.Length
            | RacemizationName -> int64 si.chiralAminoAcids.Length
            | CatalyticRacemizationName -> (int64 si.racemizationReactions.Length) * (int64 si.racemCatalysts.Length)
            | EnCatalyticRacemizationName -> (int64 si.racemizationReactions.Length) * (int64 si.enRacemCatalysts.Length) * sugLen * 2L
            | AcCatalyticRacemizationName -> (int64 si.racemizationReactions.Length) * (int64 si.acRacemCatalysts.Length)
            | ActivationName -> (int64 si.acSynthCatalysts.Length)

        member data.getReactions rnd rateProvider n = data.commonData.getReactions rnd rateProvider RandomChoice n

        /// Note that currently all generators share the same success number type.
        static member create rnd rateProvider (si : SubstInfo) st coll =
            let generatePairs x = generatePairs rnd x rateProvider
            let generateTriples x = generateTriples rnd x rateProvider

            let result =
                {
                    commonData =
                        {
                            substInfo = si
                            sugSynth = generatePairs (si.sugSynthInfo coll.sugSynthColl st)
                            catSynthPairs = generatePairs (si.catSynthInfo coll.catSynthColl st)
                            enCatSynth = generateTriples (si.enCatSynthInfo coll.enCatSynthColl st)
                            acCatSynth = generatePairs (si.acCatSynthInfo coll.acCatSynthColl st)
                            catDestrPairs = generatePairs (si.catDestrInfo coll.catDestrColl st)
                            enCatDestr = generateTriples (si.enCatDestrInfo coll.enCatDestrColl st)
                            acCatDestr = generatePairs (si.acCatDestrInfo coll.acCatDestrColl st)
                            catLigPairs = generatePairs (si.catLigInfo coll.catLigColl st)
                            enCatLig = generateTriples (si.enCatLigInfo coll.enCatLigColl st)
                            acFwdCatLig = generatePairs (si.acFwdCatLigInfo coll.acFwdCatLigColl st)
                            acBkwCatLig = generatePairs (si.acBkwCatLigInfo coll.acBkwCatLigColl st)
                            catRacemPairs = generatePairs (si.catRacemInfo coll.catRacemColl st)
                            enCatRacem = generateTriples (si.enCatRacemInfo coll.enCatRacemColl st)
                            acCatRacem = generatePairs (si.acCatRacemInfo coll.acCatRacemColl st)
                            sedDirPairs = generatePairs (si.sedDirInfo coll.sedDirColl st)
                            acPairs = generatePairs (si.acPairsInfo coll.acColl st)
                        }

                    collisionData = coll
                }

//            printfn $"RandomChoiceModelData.create: result.commonData.acPairs.Length = {result.commonData.acPairs.Length}."
//            result.commonData.acPairs
//            |> List.sort
//            |> List.map (fun (s, p) -> printfn $"    {s}, {p}")
//            |> ignore

            result


    type RateGenerationData =
        | RandomChoiceModel of RandomChoiceModelData

        member data.noOfRawReactions n =
            match data with
            | RandomChoiceModel m -> m.noOfRawReactions n

        member data.getReactions rnd rateProvider n =
            match data with
            | RandomChoiceModel m ->
                let _ = m.getReactions rnd rateProvider n

                let b =
                    match rateProvider.tryGetModel n |> Option.bind (fun m -> m.getAllReactions() |> Some) with
                    | Some v -> v
                    | None -> []

                b

        static member create rnd t rateProvider si st coll =
            match t with
            | RandomChoice -> RandomChoiceModelData.create rnd rateProvider si st coll |> RandomChoiceModel
