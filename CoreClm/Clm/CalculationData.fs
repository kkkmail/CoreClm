namespace Clm

open Microsoft.FSharp.NativeInterop
open OdePackInterop
open Clm.Substances
open Clm.Distributions
open Clm.ReactionRatesBase
open Clm.Reactions
open Clm.ModelParams
open Clm.ReactionTypes
open ClmSys.ContGenPrimitives

module CalculationData =

    type SedDirReagentInfo =
        {
            minSedDirChainLength : MaxPeptideLength
            maxSedDirChainLength : MaxPeptideLength
        }

        /// Default is that we want resolving agents affect all chains starting from some amino acid.
        /// That models the situation that the chain "binds" to the resolving agent ony by one of its ends.
        static member defaultValue =
            {
                minSedDirChainLength = OneMax
                maxSedDirChainLength = ThreeMax
            }


    type SedDirAgentInfo =
        {
            minSedDirAgentLength : MaxPeptideLength
            maxSedDirAgentLength : MaxPeptideLength
        }

        /// Default is that we want resolving agents of length 3 only.
        static member defaultValue =
            {
                minSedDirAgentLength = ThreeMax
                maxSedDirAgentLength = ThreeMax
            }


    type SedDirInfo =
        {
            sedDirReagentInfo : SedDirReagentInfo
            sedDirAgentInfo : SedDirAgentInfo
        }

        static member defaultValue =
            {
                sedDirReagentInfo = SedDirReagentInfo.defaultValue
                sedDirAgentInfo = SedDirAgentInfo.defaultValue
            }


    type SubstInfoParam =
        {
            maxPeptideLength : MaxPeptideLength
            numberOfAminoAcids : NumberOfAminoAcids
            sedDirInfo : SedDirInfo
        }


    let createAllSubst chiralAminoAcids  peptides peptideCatalysts =
        Substance.allSimple
        @
        (chiralAminoAcids |> List.map (fun a -> Chiral a))
        @
        (peptides |> List.map (fun p -> PeptideChain p))
        @
        (ChiralSugar.all |> List.map ChiralSug)
        @
        (peptideCatalysts |> List.map ActivatedPeptide |> List.map ActivatedPeptideChain)


    let createAllInd allSubst = allSubst |> List.mapi (fun i s -> (s, i)) |> Map.ofList
    let getPeptideCatalysts (peptides : list<Peptide>) = peptides |> List.filter (fun p -> p.length > 2)


    type SubstInfo =
        {
            infoParam : SubstInfoParam
            aminoAcids : list<AminoAcid>
            chiralAminoAcids : list<ChiralAminoAcid>
            chiralSugars : list<ChiralSugar>
            peptides : list<Peptide>
            peptideCatalysts : list<Peptide>
            synthCatalysts : list<SynthCatalyst>
            sugSynthCatalysts : list<SugCatalyst>
            enSynthCatalysts : list<EnSynthCatalyst>
            acSynthCatalysts : list<AcSynthCatalyst>
            destrCatalysts : list<DestrCatalyst>
            enDestrCatalysts : list<EnDestrCatalyst>
            acDestrCatalysts : list<AcDestrCatalyst>
            ligCatalysts : list<LigCatalyst>
            enLigCatalysts : list<EnLigCatalyst>
            acFwdLigCatalysts : list<AcFwdLigCatalyst>
            acBkwLigCatalysts : list<AcBkwLigCatalyst>
            ligationPairs : list<LigationReaction>
            racemCatalysts : list<RacemizationCatalyst>
            enRacemCatalysts : list<EnRacemCatalyst>
            acRacemCatalysts : list<AcRacemCatalyst>

            sedDirReagents : Map<AminoAcid, list<SedDirReagent>>
            sedDirAgents : list<SedDirAgent>

            allChains : list<list<ChiralAminoAcid>>
            allSubst : list<Substance>
            allInd : Map<Substance, int>
            allNamesMap : Map<Substance, string>
        }

        static member create (p : SubstInfoParam) =
            let peptides = Peptide.getPeptides p.maxPeptideLength p.numberOfAminoAcids
            let chiralAminoAcids = ChiralAminoAcid.getAminoAcids p.numberOfAminoAcids
            let allChains = (chiralAminoAcids |> List.map (fun a -> [ a ])) @ (peptides |> List.map (fun p -> p.aminoAcids))
            let allLigChains = allChains |> List.filter(fun a -> a.Length < p.maxPeptideLength.length)
            let aminoAcids = AminoAcid.getAminoAcids p.numberOfAminoAcids
            let peptideCatalysts = getPeptideCatalysts peptides
            let allSubst = createAllSubst chiralAminoAcids peptides peptideCatalysts

            let reagents =
                allChains
                |> List.filter(fun a -> a.Length >= p.sedDirInfo.sedDirReagentInfo.minSedDirChainLength.length && a.Length <= p.sedDirInfo.sedDirReagentInfo.maxSedDirChainLength.length)
                |> List.map (fun e -> SedDirReagent e)

            let simReagents a = reagents |> List.filter (fun e -> (e.startsWith (L a)) || e.startsWith (R a))

            let ligationPairs =
                List.allPairs allLigChains allLigChains
                |> List.filter (fun (a, b) -> a.Length + b.Length <= p.maxPeptideLength.length)
                |> List.distinct
                |> List.sort
                |> List.map LigationReaction

            {
                infoParam = p
                aminoAcids = aminoAcids
                chiralAminoAcids = chiralAminoAcids
                chiralSugars = ChiralSugar.all
                peptides = peptides
                peptideCatalysts = peptideCatalysts
                synthCatalysts = peptideCatalysts |> List.map (fun p -> SynthCatalyst p)
                sugSynthCatalysts = peptideCatalysts |> List.map (fun p -> SugCatalyst p)
                enSynthCatalysts = peptideCatalysts |> List.map (fun p -> EnSynthCatalyst p)
                acSynthCatalysts = peptideCatalysts |> List.map (fun p -> p |> ActivatedPeptide |> AcSynthCatalyst)
                destrCatalysts = peptideCatalysts |> List.map (fun p -> DestrCatalyst p)
                enDestrCatalysts = peptideCatalysts |> List.map (fun p -> EnDestrCatalyst p)
                acDestrCatalysts = peptideCatalysts |> List.map (fun p -> p |> ActivatedPeptide |> AcDestrCatalyst)
                ligCatalysts = peptideCatalysts |> List.map (fun p -> LigCatalyst p)
                enLigCatalysts = peptideCatalysts |> List.map (fun p -> EnLigCatalyst p)
                acFwdLigCatalysts = peptideCatalysts |> List.map (fun p -> p |> ActivatedPeptide |> AcFwdLigCatalyst)
                acBkwLigCatalysts = peptideCatalysts |> List.map (fun p -> p |> ActivatedPeptide |> AcBkwLigCatalyst)
                ligationPairs = ligationPairs
                racemCatalysts = peptideCatalysts |> List.map (fun p -> RacemizationCatalyst p)
                enRacemCatalysts = peptideCatalysts |> List.map (fun p -> EnRacemCatalyst p)
                acRacemCatalysts = peptideCatalysts |> List.map (fun p -> p |> ActivatedPeptide |> AcRacemCatalyst)
                sedDirReagents = aminoAcids |> List.map (fun a -> (a, simReagents a)) |> Map.ofList

                sedDirAgents =
                    allChains
                    |> List.filter(fun a -> a.Length >= p.sedDirInfo.sedDirAgentInfo.minSedDirAgentLength.length && a.Length <= p.sedDirInfo.sedDirAgentInfo.maxSedDirAgentLength.length)
                    |> List.map (fun e -> SedDirAgent e)

                allChains = allChains
                allSubst = allSubst
                allInd = createAllInd allSubst
                allNamesMap = allSubst |> List.map (fun s -> s, s.name) |> Map.ofList
            }

        member si.synthesisReactions = si.chiralAminoAcids |> List.map SynthesisReaction
        member si.destructionReactions = si.chiralAminoAcids |> List.map DestructionReaction
        member si.ligationReactions = si.ligationPairs
        member si.racemizationReactions = si.chiralAminoAcids |> List.map RacemizationReaction

        member si.sugSynthInfo i t =
            {
                pairCollision = i
                a = si.chiralSugars |> Array.ofList
                b = si.sugSynthCatalysts |> Array.ofList
                getEnantiomerA = getEnantiomer
                getEnantiomerB = getEnantiomer
                reactionName = ReactionName.SugarSynthesisName
                successNumberType = t
            }

        member si.acPairsInfo i t =
            {
                pairCollision = i

                a = si.chiralSugars |> Array.ofList
                b = si.peptideCatalysts |> Array.ofList
                getEnantiomerA = getEnantiomer
                getEnantiomerB = getEnantiomer
                reactionName = ReactionName.ActivationName
                successNumberType = t
            }

        member si.catSynthInfo i t =
            {
                pairCollision = i
                a = si.synthesisReactions |> Array.ofList
                b = si.synthCatalysts |> Array.ofList
                getEnantiomerA = getEnantiomer
                getEnantiomerB = getEnantiomer
                reactionName = ReactionName.CatalyticSynthesisName
                successNumberType = t
            }

        member si.enCatSynthInfo i t =
            {
                tripleCollision = i
                a = si.synthesisReactions |> Array.ofList
                b = si.enSynthCatalysts |> Array.ofList
                c = si.chiralSugars |> Array.ofList
                getEnantiomerA = getEnantiomer
                getEnantiomerB = getEnantiomer
                getEnantiomerC = getEnantiomer
                reactionName = ReactionName.EnCatalyticSynthesisName
                successNumberType = t
            }

        member si.acCatSynthInfo i t =
            {
                pairCollision = i
                a = si.synthesisReactions |> Array.ofList
                b = si.acSynthCatalysts |> Array.ofList
                getEnantiomerA = getEnantiomer
                getEnantiomerB = getEnantiomer
                reactionName = ReactionName.AcCatalyticSynthesisName
                successNumberType = t
            }

        member si.catDestrInfo i t =
            {
                pairCollision = i
                a = si.destructionReactions |> Array.ofList
                b = si.destrCatalysts |> Array.ofList
                getEnantiomerA = getEnantiomer
                getEnantiomerB = getEnantiomer
                reactionName = ReactionName.CatalyticDestructionName
                successNumberType = t
            }

        member si.enCatDestrInfo i t =
            {
                tripleCollision = i
                a = si.destructionReactions |> Array.ofList
                b = si.enDestrCatalysts |> Array.ofList
                c = si.chiralSugars |> Array.ofList
                getEnantiomerA = getEnantiomer
                getEnantiomerB = getEnantiomer
                getEnantiomerC = getEnantiomer
                reactionName = ReactionName.EnCatalyticDestructionName
                successNumberType = t
            }

        member si.acCatDestrInfo i t =
            {
                pairCollision = i
                a = si.destructionReactions |> Array.ofList
                b = si.acDestrCatalysts |> Array.ofList
                getEnantiomerA = getEnantiomer
                getEnantiomerB = getEnantiomer
                reactionName = ReactionName.AcCatalyticDestructionName
                successNumberType = t
            }

        member si.catLigInfo i t =
            {
                pairCollision = i
                a = si.ligationReactions |> Array.ofList
                b = si.ligCatalysts |> Array.ofList
                getEnantiomerA = getEnantiomer
                getEnantiomerB = getEnantiomer
                reactionName = ReactionName.CatalyticLigationName
                successNumberType = t
            }

        member si.enCatLigInfo i t =
            {
                tripleCollision = i
                a = si.ligationReactions |> Array.ofList
                b = si.enLigCatalysts |> Array.ofList
                c = si.chiralSugars |> Array.ofList
                getEnantiomerA = getEnantiomer
                getEnantiomerB = getEnantiomer
                getEnantiomerC = getEnantiomer
                reactionName = ReactionName.EnCatalyticLigationName
                successNumberType = t
            }

        member si.acFwdCatLigInfo i t =
            {
                pairCollision = i
                a = si.ligationReactions |> Array.ofList
                b = si.acFwdLigCatalysts |> Array.ofList
                getEnantiomerA = getEnantiomer
                getEnantiomerB = getEnantiomer
                reactionName = ReactionName.AcFwdCatalyticLigationName
                successNumberType = t
            }

        member si.acBkwCatLigInfo i t =
            {
                pairCollision = i
                a = si.ligationReactions |> Array.ofList
                b = si.acBkwLigCatalysts |> Array.ofList
                getEnantiomerA = getEnantiomer
                getEnantiomerB = getEnantiomer
                reactionName = ReactionName.AcBkwCatalyticLigationName
                successNumberType = t
            }

        member si.catRacemInfo i t =
            {
                pairCollision = i
                a = si.racemizationReactions |> Array.ofList
                b = si.racemCatalysts |> Array.ofList
                getEnantiomerA = getEnantiomer
                getEnantiomerB = getEnantiomer
                reactionName = ReactionName.CatalyticRacemizationName
                successNumberType = t
            }

        member si.enCatRacemInfo i t =
            {
                tripleCollision = i
                a = si.racemizationReactions |> Array.ofList
                b = si.enRacemCatalysts |> Array.ofList
                c = si.chiralSugars |> Array.ofList
                getEnantiomerA = getEnantiomer
                getEnantiomerB = getEnantiomer
                getEnantiomerC = getEnantiomer
                reactionName = ReactionName.EnCatalyticRacemizationName
                successNumberType = t
            }

        member si.acCatRacemInfo i t =
            {
                pairCollision = i
                a = si.racemizationReactions |> Array.ofList
                b = si.acRacemCatalysts |> Array.ofList
                getEnantiomerA = getEnantiomer
                getEnantiomerB = getEnantiomer
                reactionName = ReactionName.AcCatalyticRacemizationName
                successNumberType = t
            }

        member si.sedDirInfo i t =
            {
                pairCollision = i
                a = si.chiralAminoAcids |> Array.ofList
                b = si.sedDirAgents |> Array.ofList
                getEnantiomerA = getEnantiomer
                getEnantiomerB = getEnantiomer
                reactionName = ReactionName.SedimentationDirectName
                successNumberType = t
            }


    type LevelZero = double
    type LevelOne = double * int
    type LevelTwo = double * int * int
    type LevelThree = double * int * int * int
    type LevelFour = double * int * int * int * int


    type SubstUpdateInfo =
        | NoSubst
        | OneSubst of Substance
        | TwoSubst of Substance * Substance
        | ThreeSubst of Substance * Substance * Substance
        | FourSubst of Substance * Substance * Substance * Substance

        static member create i =
            match i with
            | [] -> NoSubst
            | h1 :: t1 ->
                match t1 with
                | [] -> h1 |> OneSubst
                | h2 :: t2 ->
                    match t2 with
                    | [] -> (h1, h2) |> TwoSubst
                    | h3 :: t3 ->
                        match t3 with
                        | [] -> (h1, h2, h3) |> ThreeSubst
                        | h4 :: t4 ->
                            match t4 with
                            | [] -> (h1, h2, h3, h4) |> FourSubst
                            | _ -> failwith (sprintf "SubstUpdateInfo: invalid input: %A" i)


    type ModelIndices =
        {
            level0 : array<LevelZero>
            level1 : array<LevelOne>
            level2 : array<LevelTwo>
            level3 : array<LevelThree>
            level4 : array<LevelFour>
        }

        static member defaultValue =
            {
                level0 = [||]
                level1 = [||]
                level2 = [||]
                level3 = [||]
                level4 = [||]
            }

        static member create (m : Map<Substance, int>) (i : list<double * SubstUpdateInfo>) =
            let l0 =
                i
                |> List.map (fun (v, e) -> match e with | NoSubst -> Some v | _ -> None)
                |> List.choose id

            let l1 =
                i
                |> List.map (fun (v, e) -> match e with | OneSubst s1 -> Some (v, m.[s1]) | _ -> None)
                |> List.choose id

            let l2 =
                i
                |> List.map (fun (v, e) -> match e with | TwoSubst (s1, s2) -> Some (v, m.[s1], m.[s2]) | _ -> None)
                |> List.choose id

            let l3 =
                i
                |> List.map (fun (v, e) -> match e with | ThreeSubst (s1, s2, s3) -> Some (v, m.[s1], m.[s2], m.[s3]) | _ -> None)
                |> List.choose id

            let l4 =
                i
                |> List.map (fun (v, e) -> match e with | FourSubst (s1, s2, s3, s4) -> Some (v, m.[s1], m.[s2], m.[s3], m.[s4]) | _ -> None)
                |> List.choose id

            {
                level0 = l0 |> Array.ofList
                level1 = l1 |> Array.ofList
                level2 = l2 |> Array.ofList
                level3 = l3 |> Array.ofList
                level4 = l4 |> Array.ofList
            }


    let calculateTotalSubst (totalSubst : array<LevelOne>) (x: double[]) =
        let mutable sum = 0.0

        for coeff, j1 in totalSubst do
            sum <- sum + coeff * x.[j1]

        sum


    let calculateTotals (totals : array<array<LevelOne> * array<LevelOne>>) (x: double[]) =
        totals
        |> Array.map (fun (l, r) -> calculateTotalSubst l x, calculateTotalSubst r x)


    let calculateDerivativeValue (x: double[]) (indices : ModelIndices) : double =
        let mutable sum = 0.0

        for coeff in indices.level0 do
            sum <- sum + coeff

        for coeff, j1 in indices.level1 do
            sum <- sum + coeff * x.[j1]

        for coeff, j1, j2 in indices.level2 do
            sum <- sum + coeff * x.[j1] * x.[j2]

        for coeff, j1, j2, j3 in indices.level3 do
            sum <- sum + coeff * x.[j1] * x.[j2] * x.[j3]

        for coeff, j1, j2, j3, j4 in indices.level4 do
            sum <- sum + coeff * x.[j1] * x.[j2] * x.[j3] * x.[j4]

        sum


    let calculateByRefDerivativeValue (x : nativeptr<double>) (indices : ModelIndices) (dx : nativeptr<double>) (idx : int) : unit =
        let mutable sum = 0.0

        for coeff in indices.level0 do
            sum <- sum + coeff

        for coeff, j1 in indices.level1 do
            sum <- sum + coeff * (NativePtr.get x j1)

        for coeff, j1, j2 in indices.level2 do
            sum <- sum + coeff * (NativePtr.get x j1) * (NativePtr.get x j2)

        for coeff, j1, j2, j3 in indices.level3 do
            sum <- sum + coeff * (NativePtr.get x j1) * (NativePtr.get x j2) * (NativePtr.get x j3)

        for coeff, j1, j2, j3, j4 in indices.level4 do
            sum <- sum + coeff * (NativePtr.get x j1) * (NativePtr.get x j2) * (NativePtr.get x j3) * (NativePtr.get x j4)

        NativePtr.set dx idx sum



    let makeNonNegative (x: double[]) = x |> Array.map (max 0.0)
    let makeNonNegativeByRef (neq : int) (x : nativeptr<double>) : double[] = [| for i in 0.. neq - 1 -> max 0.0 (NativePtr.get x i) |]

    let makeNonNegativeByRefInPlace (neq : int) (x : nativeptr<double>) =
        for i in 0 .. (neq - 1) do
            NativePtr.set x i (max 0.0 (NativePtr.get x i))


    let private f (callaBack: double -> double[] -> unit, indices : array<ModelIndices>, neq : byref<int>, t : byref<double>, x : nativeptr<double>, dx : nativeptr<double>) : unit =
        let x1 = makeNonNegativeByRef neq x
        callaBack t x1

        for i in 0 .. (neq - 1) do
            NativePtr.set dx i (calculateDerivativeValue x1 indices.[i])


    let createInterop (callaBack: double -> double[] -> unit) (indices : array<ModelIndices>) =
        Interop.F(fun n t y dy -> f(callaBack, indices, &n, &t, y, dy))


    type ModelCalculationData =
        {
            totalSubst : array<LevelOne>
            totals : array<array<LevelOne> * array<LevelOne>>
            derivative : array<ModelIndices>
        }


        member md.createInterop c : Interop.F = createInterop c md.derivative

        static member defaultValue =
            {
                totalSubst = [||]
                totals = [||]
                derivative = [||]
            }

        member md.getDerivative x =
            let y = makeNonNegative x
            md.derivative |> Array.map (calculateDerivativeValue y)

        member md.getTotalSubst x = x |> makeNonNegative |> calculateTotalSubst md.totalSubst
        member md.getTotals x = x |> makeNonNegative |> calculateTotals md.totals

        static member createTotalSubst (si : SubstInfo) =
            si.allSubst
            |> List.map (fun s -> (double s.atoms, si.allInd.[s] ))
            |> Array.ofList

        static member createTotals (si : SubstInfo) : array<array<LevelOne> * array<LevelOne>> =
            let g a =
                si.allSubst
                |> List.map (fun s -> match s.noOfAminoAcid a with | Some i -> Some (double i, si.allInd.[s]) | None -> None)
                |> List.choose id
                |> Array.ofList

            let y =
                si.aminoAcids
                |> List.map (fun a -> L a |> g, R a |> g)
                |> Array.ofList

            y

        static member createDerivative (si : SubstInfo) (allReac : list<AnyReaction>) =
            let normalized = allReac |> List.map (fun e -> e.reaction.info.normalized(), e.forwardRate, e.backwardRate)

            let processReaction i o (ReactionRate v) =
                let r = i |> SubstUpdateInfo.create

                (i |> List.map(fun e -> e, -1))
                @
                (o |> List.map(fun e -> e, 1))
                |> List.groupBy fst
                |> List.map (fun (s, e) -> s, e |> List.map snd |> List.sum)
                |> List.filter (fun (_, e) -> e <> 0)
                |> List.map (fun (s, m) -> s, ((double m) * v, r))

            let getRates chooser =
                normalized
                |> List.map (fun (r, f, b) -> r, chooser (f, b))
                |> List.choose (fun (e, r) -> r |> Option.bind (fun v -> Some (e, v)))

            let allMap =
                (getRates fst |> List.map (fun (r, v) -> processReaction r.inputNormalized r.outputNormalized v))
                @
                (getRates snd |> List.map (fun (r, v) -> processReaction r.outputNormalized r.inputNormalized v))
                |> List.concat
                |> List.groupBy (fun (s, _) -> s)
                |> List.map (fun (s, l) -> si.allInd.[s], l |> List.map (fun (_, e) -> e) |> ModelIndices.create si.allInd)
                |> Map.ofList

            [| for i in 0..(si.allSubst.Length - 1) -> allMap.TryFind i |]
            |> Array.map (fun e -> match e with | Some v -> v | None -> ModelIndices.defaultValue)

        static member create (si : SubstInfo) (allReac : list<AnyReaction>) =
            {
                totalSubst = ModelCalculationData.createTotalSubst si
                totals = ModelCalculationData.createTotals si
                derivative = ModelCalculationData.createDerivative si allReac
            }


    type ModelBinaryData =
        {
            calculationData : ModelCalculationData
            allRawReactions : list<ReactionName * int64>
            allReactions : list<ReactionName * int64>
        }


    type ModelAllData =
        {
            modelDataParams : ModelDataParams
            modelBinaryData : ModelBinaryData
        }

        member this.getModelDataParamsWithExtraData() : ModelDataParamsWithExtraData =
            let numberOfAminoAcids = this.modelDataParams.modelInfo.numberOfAminoAcids
            let maxPeptideLength = this.modelDataParams.modelInfo.maxPeptideLength
            let chiralAminoAcids = ChiralAminoAcid.getAminoAcids numberOfAminoAcids
            let peptides = Peptide.getPeptides maxPeptideLength numberOfAminoAcids
            let peptideCatalysts = getPeptideCatalysts peptides
            let allSubst = createAllSubst chiralAminoAcids peptides peptideCatalysts
            let allInd = createAllInd allSubst

            {
                regularParams =
                    {
                        modelDataParams = this.modelDataParams
                        allSubstData =
                            {
                                allSubst = allSubst
                                allInd = allInd
                                allRawReactions = this.modelBinaryData.allRawReactions
                                allReactions = this.modelBinaryData.allReactions
                            }
                    }

                funcParams =
                    {
                        getTotals = this.modelBinaryData.calculationData.getTotals
                        getTotalSubst = this.modelBinaryData.calculationData.getTotalSubst
                        getDerivative = this.modelBinaryData.calculationData.getDerivative
                    }
            }


    type ModelDataRaw =
        {
            seedValue : int option
            fileStructureVersion : decimal
            modelData : ModelAllData
        }


    type ModelData =
        {
            modelDataId : ModelDataId
            clmTaskInfo : ClmTaskInfo
            data : ModelDataRaw
        }

        member this.seedValue = this.data.seedValue
        member this.fileStructureVersion = this.data.fileStructureVersion
        member this.modelData = this.data.modelData
