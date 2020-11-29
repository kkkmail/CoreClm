namespace Clm

open Substances

module ReactionTypes =

    let toSubstName a = a |> List.fold (fun acc r -> acc + r.ToString()) ""

    type ReactionName =
        | FoodCreationName
        | WasteRemovalName
        | WasteRecyclingName
        | SynthesisName
        | SugarSynthesisName
        | DestructionName
        | CatalyticSynthesisName
        | EnCatalyticSynthesisName
        | CatalyticDestructionName
        | EnCatalyticDestructionName
        | LigationName
        | CatalyticLigationName
        | EnCatalyticLigationName
        | SedimentationDirectName
        | SedimentationAllName
        | RacemizationName
        | CatalyticRacemizationName
        | EnCatalyticRacemizationName

        member this.name =
            match this with
            | FoodCreationName -> "food"
            | WasteRemovalName -> "waste"
            | WasteRecyclingName -> "recycling"
            | SynthesisName -> "synthesis"
            | SugarSynthesisName -> "sugar synthesis"
            | DestructionName -> "destruction"
            | CatalyticSynthesisName -> "catalytic synthesis"
            | EnCatalyticSynthesisName -> "catalytic synthesis with energy consumption"
            | CatalyticDestructionName -> "catalytic destruction"
            | EnCatalyticDestructionName -> "catalytic destruction with energy consumption"
            | LigationName -> "ligation"
            | CatalyticLigationName -> "catalytic ligation"
            | EnCatalyticLigationName -> "catalytic ligation with energy consumption"
            | SedimentationDirectName -> "sedimentation direct"
            | SedimentationAllName -> "sedimentation all"
            | RacemizationName -> "racemization"
            | CatalyticRacemizationName -> "catalytic racemization"
            | EnCatalyticRacemizationName -> "catalytic racemization with energy consumption"

        static member all =
            [
                FoodCreationName
                WasteRemovalName
                WasteRecyclingName
                SynthesisName
                SugarSynthesisName
                DestructionName
                CatalyticSynthesisName
                EnCatalyticSynthesisName
                CatalyticDestructionName
                EnCatalyticDestructionName
                LigationName
                CatalyticLigationName
                EnCatalyticLigationName
                SedimentationDirectName
                SedimentationAllName
                RacemizationName
                CatalyticRacemizationName
                EnCatalyticRacemizationName
            ]


    type ReactionNormalizedInfo =
        {
            inputNormalized : list<Substance>
            outputNormalized : list<Substance>
        }


    type ReactionInfo =
        {
            input : list<Substance * int>
            output : list<Substance * int>
        }

        member this.getName n a d =
            let g (l : list<Substance * int>) =
                l
                |> List.map (fun (s, n) -> (if n = 1 then "" else n.ToString() + " ") + s.name)
                |> String.concat " + "

            let b =
                match d with
                | Some v -> " " + v
                | None -> ""

            n + b + ": " + (g this.input) + a + (g this.output)

        member this.normalized() =
            let normalize d =
                d
                |> List.map (fun (s, i) -> [ for _ in 0..(i-1) -> s ])
                |> List.concat
                |> List.sort

            {
                inputNormalized = this.input |> normalize
                outputNormalized = this.output |> normalize
            }


    type FoodCreationReaction =
        | FoodCreationReaction

        member r.info =
            {
                input = [ (Simple Abundant, 0) ]
                output = [ (Simple Food, 1) ]
            }

        member r.enantiomer = r


    type WasteRemovalReaction =
        | WasteRemovalReaction

        member r.info =
            {
                input = [ (Simple Waste, 1) ]
                output = []
            }

        member r.enantiomer = r


    type WasteRecyclingReaction =
        | WasteRecyclingReaction

        member r.info =
            {
                input = [ (Simple Waste, 1) ]
                output = [ (Simple Food, 1) ]
            }

        member r.enantiomer = r


    type SynthesisReaction =
        | SynthesisReaction of ChiralAminoAcid

        member r.info =
            let (SynthesisReaction a) = r
            {
                input = [ (Simple Food, 1) ]
                output = [ (Chiral a, 1) ]
            }

        member r.enantiomer =
            let (SynthesisReaction a) = r
            a.enantiomer |> SynthesisReaction


    type SugCatalyst =
        | SugCatalyst of Peptide

        member c.enantiomer =
            let (SugCatalyst a) = c
            a.enantiomer |> SugCatalyst


    type SugarSynthesisReaction =
        | SugarSynthesisReaction of ChiralSugar * SugCatalyst

        member r.info =
            let (SugarSynthesisReaction (a, (SugCatalyst c))) = r
            let p = c |> PeptideChain

            {
                input = [ (Simple Food, 1); (p, 1) ]
                output = [ (ChiralSug a, 1); (p, 1) ]
            }

        member r.enantiomer =
            let (SugarSynthesisReaction (a, c)) = r
            (a.enantiomer, c.enantiomer) |> SugarSynthesisReaction

        member r.withEnantiomerCatalyst =
            let (SugarSynthesisReaction (a, c)) = r
            (a, c.enantiomer) |> SugarSynthesisReaction

        member r.catalyst =
            let (SugarSynthesisReaction (_, b)) = r
            b


    type DestructionReaction =
        | DestructionReaction of ChiralAminoAcid

        member r.info =
            let (DestructionReaction a) = r
            {
                input = [ (Chiral a, 1) ]
                output = [ (Simple Waste, 1) ]
            }

        member r.enantiomer =
            let (DestructionReaction a) = r
            a.enantiomer |> DestructionReaction


    type SynthCatalyst =
        | SynthCatalyst of Peptide

        member c.enantiomer =
            let (SynthCatalyst a) = c
            a.enantiomer |> SynthCatalyst


    type CatalyticSynthesisReaction =
        | CatalyticSynthesisReaction of (SynthesisReaction * SynthCatalyst)

        member r.info =
            let (CatalyticSynthesisReaction (a, (SynthCatalyst c))) = r
            let p = c |> PeptideChain

            {
                input = a.info.input @ [ (p, 1) ]
                output = a.info.output @ [ (p, 1) ]
            }

        member r.enantiomer =
            let (CatalyticSynthesisReaction (a, c)) = r
            (a.enantiomer, c.enantiomer) |> CatalyticSynthesisReaction

        member r.withEnantiomerCatalyst =
            let (CatalyticSynthesisReaction (a, c)) = r
            (a, c.enantiomer) |> CatalyticSynthesisReaction

        member r.baseReaction =
            let (CatalyticSynthesisReaction (a, _)) = r
            a

        member r.catalyst =
            let (CatalyticSynthesisReaction (_, b)) = r
            b


    /// Catalyst, which works in synthesis reaction with energy consumption.
    type EnSynthCatalyst =
        | EnSynthCatalyst of Peptide

        member c.enantiomer =
            let (EnSynthCatalyst a) = c
            a.enantiomer |> EnSynthCatalyst


    type EnCatalyticSynthesisReaction =
        | EnCatalyticSynthesisReaction of (SynthesisReaction * EnSynthCatalyst * ChiralSugar)

        member r.info =
            let (EnCatalyticSynthesisReaction (a, (EnSynthCatalyst c), s)) = r
            let p = c |> PeptideChain

            {
                input = a.info.input @ [ (p, 1); (ChiralSug s, 1) ]
                output = a.info.output @ [ (p, 1); (Simple Waste, 1) ]
            }

        member r.enantiomer =
            let (EnCatalyticSynthesisReaction (a, c, s)) = r
            (a.enantiomer, c.enantiomer, s.enantiomer) |> EnCatalyticSynthesisReaction

        member r.withEnantiomerCatalyst =
            let (EnCatalyticSynthesisReaction (a, c, s)) = r
            (a, c.enantiomer, s) |> EnCatalyticSynthesisReaction

        member r.baseReaction =
            let (EnCatalyticSynthesisReaction (a, _, _)) = r
            a

        member r.catalyst =
            let (EnCatalyticSynthesisReaction (_, b, _)) = r
            b

        member r.sugar =
            let (EnCatalyticSynthesisReaction (_, _, c)) = r
            c


    type DestrCatalyst =
        | DestrCatalyst of Peptide

        member c.enantiomer =
            let (DestrCatalyst a) = c
            a.enantiomer |> DestrCatalyst


    type CatalyticDestructionReaction =
        | CatalyticDestructionReaction of (DestructionReaction * DestrCatalyst)

        member r.info =
            let (CatalyticDestructionReaction (a, (DestrCatalyst c))) = r
            let p = c |> PeptideChain

            {
                input = a.info.input @ [ (p, 1) ]
                output = a.info.output @ [ (p, 1) ]
            }

        member r.enantiomer =
            let (CatalyticDestructionReaction (a, c)) = r
            (a.enantiomer, c.enantiomer) |> CatalyticDestructionReaction

        member r.withEnantiomerCatalyst =
            let (CatalyticDestructionReaction (a, c)) = r
            (a, c.enantiomer) |> CatalyticDestructionReaction


        member r.baseReaction =
            let (CatalyticDestructionReaction (a, _)) = r
            a

        member r.catalyst =
            let (CatalyticDestructionReaction (_, b)) = r
            b


    /// Catalyst, which works in destruction reaction with energy consumption.
    type EnDestrCatalyst =
        | EnDestrCatalyst of Peptide

        member c.enantiomer =
            let (EnDestrCatalyst a) = c
            a.enantiomer |> EnDestrCatalyst


    type EnCatalyticDestructionReaction =
        | EnCatalyticDestructionReaction of (DestructionReaction * EnDestrCatalyst * ChiralSugar)

        member r.info =
            let (EnCatalyticDestructionReaction (a, (EnDestrCatalyst c), s)) = r
            let p = c |> PeptideChain

            {
                input = a.info.input @ [ (p, 1); (ChiralSug s, 1) ]
                output = a.info.output @ [ (p, 1); (Simple Waste, 1) ]
            }

        member r.enantiomer =
            let (EnCatalyticDestructionReaction (a, c, s)) = r
            (a.enantiomer, c.enantiomer, s.enantiomer) |> EnCatalyticDestructionReaction

        member r.withEnantiomerCatalyst =
            let (EnCatalyticDestructionReaction (a, c, s)) = r
            (a, c.enantiomer, s) |> EnCatalyticDestructionReaction

        member r.baseReaction =
            let (EnCatalyticDestructionReaction (a, _, _)) = r
            a

        member r.catalyst =
            let (EnCatalyticDestructionReaction (_, b, _)) = r
            b

        member r.sugar =
            let (EnCatalyticDestructionReaction (_, _, c)) = r
            c


    /// A directed pair of amino acids forming peptide bond.
    /// leftAminoAcid is the amino acid on the left side of the bond and
    /// rightAminoAcid is on the right side of the bond.
    /// Any of these amino acids can be L or R.
    type PeptideBond =
        {
            leftAminoAcid : ChiralAminoAcid
            rightAminoAcid : ChiralAminoAcid
        }

        member r.enantiomer = { leftAminoAcid = r.leftAminoAcid.enantiomer; rightAminoAcid = r.rightAminoAcid.enantiomer }
        override r.ToString() = "(" + r.leftAminoAcid.name + " + " + r.rightAminoAcid.name + ")"

        member r.bingingSymmetry =
            match r.leftAminoAcid, r.rightAminoAcid with
            | L _, L _ -> LL
            | L _, R _ -> LR
            | R _, L _ -> RL
            | R _, R _ -> RR


    type LigationReaction =
        | LigationReaction of (list<ChiralAminoAcid> * list<ChiralAminoAcid>)

        member this.value = let (LigationReaction v) = this in v

        member r.info =
            let (LigationReaction (a, b)) = r

            {
                input = [ (Substance.fromList a, 1); (Substance.fromList b, 1) ]
                output = [ (Substance.fromList (a @ b), 1) ]
            }

        member r.peptideBond =
            let (LigationReaction (a, b)) = r
            { leftAminoAcid = a |> List.rev |> List.head; rightAminoAcid = b |> List.head }

        member r.bingingSymmetry = r.peptideBond.bingingSymmetry

        member r.enantiomer =
            let (LigationReaction (a, b)) = r
            (a |> List.map (fun e -> e.enantiomer), b |> List.map (fun e -> e.enantiomer)) |> LigationReaction

        override r.ToString() =
            let (LigationReaction (a, b)) = r
            let sa = toSubstName a
            let sb = toSubstName b
            sprintf "LigationReaction: %s + %s <-> %s" sa sb (sa + sb)


    type PeptideBondData =
        {
            ligationReactionMap : Map<PeptideBond, List<LigationReaction>>
            peptideBondMap : Map<BindingSymmetry, List<PeptideBond>>
        }

        /// Finds all ligation reactions with the same peptide bond INCLUDING input bond.
        member m.findSameBond (x : PeptideBond) =
            m.ligationReactionMap
            |> Map.tryFind x
            |> Option.defaultValue List.empty

        member m.findSameBondSymmetry (x : PeptideBond) =
            m.peptideBondMap
            |> Map.tryFind x.bingingSymmetry
            |> Option.defaultValue List.empty

//        /// Finds all ligation reactions with the same peptide bond EXCEPT input reaction.
//        /// Enantiomers are excluded as well.
//        member m.findSameX (x : LigationReaction) =
//            let xe = x.enantiomer
//            m.findSame x |> List.filter (fun e -> e <> x && e <> xe)

//        /// Finds all ligation reactions, which have the same peptide bond symmetry as a given peptide bond
//        /// E.g. aB + C -> aBC.
//        member m.findSimilar (x : PeptideBond) =
//            m.ligationReactionMap
//            |> Map.tryFind x.bingingSymmetry
//            |> Option.defaultValue Map.empty
//            |> Map.toList
//            |> List.map snd
//            |> List.map (fun e -> e |> Set.toList)
//            |> List.concat
//            |> List.distinct
//            |> List.sortBy (fun e -> e.info)
//
//        /// Finds all ligation reactions, which have the same peptide bond symmetry as given ligation reaction (e.g. aB + C -> aBC).
//        /// But NOT the same bond. E.g. if incoming reaction is aB + C -> aBC, then peptide bond (of this reaction) is BC,
//        /// bond symmetry is LL and this function returns all ligation reactions, which have bond symmetry type LL but not bond BC.
//        /// E.g.: aB + D -> aBD, AC + E -> ACE, A + De -> ADe, etc..., but NOT B + C -> BC, B + Ce -> BCe, etc...
//        /// Enantiomers are excluded as well.
//        member m.findSimilarX (x : LigationReaction) =
//            let xp = x.peptideBond
//            let xpe = x.peptideBond.enantiomer
//            m.findSimilar x |> List.filter(fun e -> e.peptideBond <> xp && e.peptideBond <> xpe)

        static member create (p : List<LigationReaction>) =
            let a =
                p
                |> List.groupBy (fun e -> e.peptideBond)
                |> List.map (fun (a, b) -> a, b |> List.sortBy (fun e -> e.info))
                |> Map.ofList

            let b =
                p
                |> List.map (fun e -> e.peptideBond)
                |> List.distinct
                |> List.groupBy (fun e -> e.bingingSymmetry)
                |> List.map (fun (a, b) -> a, b |> List.sort)
                |> Map.ofList

            {
                ligationReactionMap = a
                peptideBondMap = b
            }

    type LigCatalyst =
        | LigCatalyst of Peptide

        member c.enantiomer =
            let (LigCatalyst a) = c
            a.enantiomer |> LigCatalyst


    type CatalyticLigationReaction =
        | CatalyticLigationReaction of (LigationReaction * LigCatalyst)

        member r.info =
            let (CatalyticLigationReaction (LigationReaction (a, b), LigCatalyst c)) = r
            let p = c |> PeptideChain

            {
                input = [ (Substance.fromList a, 1); (Substance.fromList b, 1); (p, 1) ]
                output = [ (Substance.fromList (a @ b), 1); (p, 1) ]
            }

        member r.enantiomer =
            let (CatalyticLigationReaction (l, c)) = r
            (l.enantiomer, c.enantiomer) |> CatalyticLigationReaction

        member r.baseReaction =
            let (CatalyticLigationReaction (a, _)) = r
            a

        member r.catalyst =
            let (CatalyticLigationReaction (_, b)) = r
            b

        member r.withEnantiomerCatalyst =
            let (CatalyticLigationReaction (a, c)) = r
            (a, c.enantiomer) |> CatalyticLigationReaction

        override r.ToString() =
            let (CatalyticLigationReaction (LigationReaction (a, b), LigCatalyst (Peptide c))) = r
            let sa = toSubstName a
            let sb = toSubstName b
            let sc = toSubstName c
            sprintf "CatalyticLigationReaction: %s + %s + %s <-> %s + %s" sa sb sc (sa + sb) sc


    /// Catalyst, which works in ligation reaction with energy consumption.
    type EnLigCatalyst =
        | EnLigCatalyst of Peptide

        member c.enantiomer =
            let (EnLigCatalyst a) = c
            a.enantiomer |> EnLigCatalyst


    type EnCatalyticLigationReaction =
        | EnCatalyticLigationReaction of (LigationReaction * EnLigCatalyst *  ChiralSugar)

        member r.info =
            let (EnCatalyticLigationReaction (LigationReaction (a, b), EnLigCatalyst c, s)) = r
            let p = c |> PeptideChain

            {
                input = [ (Substance.fromList a, 1); (Substance.fromList b, 1); (p, 1); (ChiralSug s, 1) ]
                output = [ (Substance.fromList (a @ b), 1); (p, 1); (Simple Waste, 1) ]
            }

        member r.enantiomer =
            let (EnCatalyticLigationReaction (l, c, s)) = r
            (l.enantiomer, c.enantiomer, s.enantiomer) |> EnCatalyticLigationReaction

        member r.baseReaction =
            let (EnCatalyticLigationReaction (a, _, _)) = r
            a

        member r.catalyst =
            let (EnCatalyticLigationReaction (_, b, _)) = r
            b

        member r.sugar =
            let (EnCatalyticLigationReaction (_, _, c)) = r
            c

        member r.withEnantiomerCatalyst =
            let (EnCatalyticLigationReaction (a, c, s)) = r
            (a, c.enantiomer, s) |> EnCatalyticLigationReaction

        override r.ToString() =
            let (EnCatalyticLigationReaction (LigationReaction (a, b), EnLigCatalyst (Peptide c), s)) = r
            let sa = toSubstName a
            let sb = toSubstName b
            let sc = toSubstName c
            let ss = s.name
            sprintf "EnCatalyticLigationReaction: %s + %s + %s + %s <-> %s + %s + %s" sa sb sc ss (sa + sb) sc "waste"


    /// A resolving agent, which forms insoluble diasteriomeric salt with one of the enantiomer of some amino acid (or, in general, peptide as well).
    type SedDirAgent =
        | SedDirAgent of list<ChiralAminoAcid>

        member c.enantiomer =
            let (SedDirAgent a) = c
            a |> List.map (fun e -> e.enantiomer) |> SedDirAgent

        member this.value = let (SedDirAgent v) = this in v


    /// A peptide chain, which attaches to the resolving reagent by one of its ends (list head).
    type SedDirReagent =
        | SedDirReagent of list<ChiralAminoAcid>

        member c.enantiomer =
            let (SedDirReagent a) = c
            a |> List.map (fun e -> e.enantiomer) |> SedDirReagent

        member c.value = let (SedDirReagent v) = c in v

        member c.startsWith a =
            match c.value with
            | h :: _ -> h = a
            | [] -> false


    type SedimentationDirectReaction =
        | SedimentationDirectReaction of (SedDirReagent * SedDirAgent)

        member r.info =
            let (SedimentationDirectReaction (a, b)) = r

            {
                input = [ (Substance.fromList a.value, 1); (Substance.fromList b.value, 1) ]
                output = [ (AchiralSubst.Waste |> Simple, a.value.Length + b.value.Length) ]
            }

        member r.enantiomer =
            let (SedimentationDirectReaction (a, b)) = r
            (a.enantiomer, b.enantiomer) |> SedimentationDirectReaction


    type SedimentationAllReaction =
        | SedimentationAllReaction

        member r.info =
            {
                input = []
                output = []
            }

        member r.enantiomer = r


    type RacemizationReaction =
        | RacemizationReaction of ChiralAminoAcid

        member r.info =
            let (RacemizationReaction a) = r
            {
                input = [ (Chiral a, 1) ]
                output = [ (Chiral a.enantiomer, 1) ]
            }

        member r.enantiomer =
            let (RacemizationReaction a) = r
            a.enantiomer |> RacemizationReaction


    type RacemizationCatalyst =
        | RacemizationCatalyst of Peptide

        member c.enantiomer =
            let (RacemizationCatalyst a) = c
            a.enantiomer |> RacemizationCatalyst


    type CatalyticRacemizationReaction =
        | CatalyticRacemizationReaction of (RacemizationReaction * RacemizationCatalyst)

        member r.info =
            let (CatalyticRacemizationReaction ((RacemizationReaction a), (RacemizationCatalyst c))) = r
            let p = c |> PeptideChain
            {
                input = [ (Chiral a, 1); (p, 1) ]
                output = [ (Chiral a.enantiomer, 1); (p, 1) ]
            }

        member r.enantiomer =
            let (CatalyticRacemizationReaction (a, c)) = r
            (a.enantiomer, c.enantiomer) |> CatalyticRacemizationReaction

        member r.baseReaction =
            let (CatalyticRacemizationReaction (a, _)) = r
            a

        member r.catalyst =
            let (CatalyticRacemizationReaction (_, b)) = r
            b


    /// Catalyst, which works in racemization reaction with energy consumption.
    type EnRacemCatalyst =
        | EnRacemCatalyst of Peptide

        member c.enantiomer =
            let (EnRacemCatalyst a) = c
            a.enantiomer |> EnRacemCatalyst


    type EnCatalyticRacemizationReaction =
        | EnCatalyticRacemizationReaction of (RacemizationReaction * EnRacemCatalyst * ChiralSugar)

        member r.info =
            let (EnCatalyticRacemizationReaction (a, (EnRacemCatalyst c), s)) = r
            let p = c |> PeptideChain

            {
                input = a.info.input @ [ (p, 1); (ChiralSug s, 1) ]
                output = a.info.output @ [ (p, 1); (Simple Waste, 1) ]
            }

        member r.enantiomer =
            let (EnCatalyticRacemizationReaction (a, c, s)) = r
            (a.enantiomer, c.enantiomer, s.enantiomer) |> EnCatalyticRacemizationReaction

        member r.withEnantiomerCatalyst =
            let (EnCatalyticRacemizationReaction (a, c, s)) = r
            (a, c.enantiomer, s) |> EnCatalyticRacemizationReaction

        member r.baseReaction =
            let (EnCatalyticRacemizationReaction (a, _, _)) = r
            a

        member r.catalyst =
            let (EnCatalyticRacemizationReaction (_, b, _)) = r
            b

        member r.sugar =
            let (EnCatalyticRacemizationReaction (_, _, c)) = r
            c


    let inline getName i = ((^T) : (member name : 'T) (i))
    let inline getInfo i = ((^T) : (member info : 'T) (i))


    type Reaction =
        | FoodCreation of FoodCreationReaction
        | WasteRemoval of WasteRemovalReaction
        | WasteRecycling of WasteRecyclingReaction
        | Synthesis of SynthesisReaction
        | SugarSynthesis of SugarSynthesisReaction
        | Destruction of DestructionReaction
        | CatalyticSynthesis of CatalyticSynthesisReaction
        | EnCatalyticSynthesis of EnCatalyticSynthesisReaction
        | CatalyticDestruction of CatalyticDestructionReaction
        | EnCatalyticDestruction of EnCatalyticDestructionReaction
        | Ligation of LigationReaction
        | CatalyticLigation of CatalyticLigationReaction
        | EnCatalyticLigation of EnCatalyticLigationReaction
        | SedimentationDirect of SedimentationDirectReaction
        | SedimentationAll of SedimentationAllReaction
        | Racemization of RacemizationReaction
        | CatalyticRacemization of CatalyticRacemizationReaction
        | EnCatalyticRacemization of EnCatalyticRacemizationReaction

        member r.name =
            match r with
            | FoodCreation _ -> FoodCreationName
            | WasteRemoval _ -> WasteRemovalName
            | WasteRecycling _ -> WasteRecyclingName
            | Synthesis _ -> SynthesisName
            | SugarSynthesis _ -> SugarSynthesisName
            | Destruction _ -> DestructionName
            | CatalyticSynthesis _ -> CatalyticSynthesisName
            | EnCatalyticSynthesis _ -> EnCatalyticSynthesisName
            | CatalyticDestruction _ -> CatalyticDestructionName
            | EnCatalyticDestruction _ -> EnCatalyticDestructionName
            | Ligation _ -> LigationName
            | CatalyticLigation _ -> CatalyticLigationName
            | EnCatalyticLigation _ -> EnCatalyticLigationName
            | SedimentationDirect _ -> SedimentationDirectName
            | SedimentationAll _ -> SedimentationAllName
            | Racemization _ -> RacemizationName
            | CatalyticRacemization _ -> CatalyticRacemizationName
            | EnCatalyticRacemization _ -> EnCatalyticRacemizationName

        member r.info =
            match r with
            | FoodCreation r -> r.info
            | WasteRemoval r -> r.info
            | WasteRecycling r -> r.info
            | Synthesis r -> r.info
            | SugarSynthesis r -> r.info
            | Destruction r -> r.info
            | CatalyticSynthesis r -> r.info
            | EnCatalyticSynthesis r -> r.info
            | CatalyticDestruction r -> r.info
            | EnCatalyticDestruction r -> r.info
            | Ligation r -> r.info
            | CatalyticLigation r -> r.info
            | EnCatalyticLigation r -> r.info
            | SedimentationDirect r -> r.info
            | SedimentationAll r -> r.info
            | Racemization r -> r.info
            | CatalyticRacemization r -> r.info
            | EnCatalyticRacemization r -> r.info

        member r.enantiomer =
            match r with
            | FoodCreation r -> r.enantiomer |> FoodCreation
            | WasteRemoval r -> r.enantiomer |> WasteRemoval
            | WasteRecycling r -> r.enantiomer |> WasteRecycling
            | Synthesis r -> r.enantiomer |> Synthesis
            | SugarSynthesis r -> r.enantiomer |> SugarSynthesis
            | Destruction r -> r.enantiomer |> Destruction
            | CatalyticSynthesis r -> r.enantiomer |> CatalyticSynthesis
            | EnCatalyticSynthesis r -> r.enantiomer |> EnCatalyticSynthesis
            | CatalyticDestruction r -> r.enantiomer |> CatalyticDestruction
            | EnCatalyticDestruction r -> r.enantiomer |> EnCatalyticDestruction
            | Ligation r -> r.enantiomer |> Ligation
            | CatalyticLigation r -> r.enantiomer |> CatalyticLigation
            | EnCatalyticLigation r -> r.enantiomer |> EnCatalyticLigation
            | SedimentationDirect r -> r.enantiomer |> SedimentationDirect
            | SedimentationAll r -> SedimentationAll r // There are no enantiomers here.
            | Racemization r -> r.enantiomer |> Racemization
            | CatalyticRacemization r -> r.enantiomer |> CatalyticRacemization
            | EnCatalyticRacemization r -> r.enantiomer |> EnCatalyticRacemization

        member r.addInfo =
            match r with
            | FoodCreation _ -> None
            | WasteRemoval _ -> None
            | WasteRecycling _ -> None
            | Synthesis _ -> None
            | SugarSynthesis _ -> None
            | Destruction _ -> None
            | CatalyticSynthesis _ -> None
            | EnCatalyticSynthesis _ -> None
            | CatalyticDestruction _ -> None
            | EnCatalyticDestruction _ -> None
            | Ligation r -> r.peptideBond.ToString() |> Some
            | CatalyticLigation r -> r.baseReaction.peptideBond.ToString() |> Some
            | EnCatalyticLigation r -> r.baseReaction.peptideBond.ToString() |> Some
            | SedimentationDirect _ -> None
            | SedimentationAll _ -> None
            | Racemization _ -> None
            | CatalyticRacemization _ -> None
            | EnCatalyticRacemization _ -> None
