namespace Clm

open FSharp.Collections
open Clm.Distributions
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams

module ReactionRates =
    let calculateSedDirRates (i : SedDirRatesInfo) =
        let reaction = (i.sedFormingSubst, i.sedDirAgent) |> SedimentationDirectReaction
        let re = (i.sedFormingSubst, i.sedDirAgent.enantiomer) |> SedimentationDirectReaction

        let rf, rfe =
            let k =
                match i.rateGenerationType with
                | RandomChoice -> i.eeParams.sedDirRateMultiplierDistr.nextDouble i.rnd

            match k, i.eeParams.eeDistribution with
            | Some k0, Some df ->
                let s0 = i.getBaseRates reaction
                let fEe = df.nextDouble i.rnd

                let kf = k0 * (1.0 + fEe)
                let kfe = k0 * (1.0 - fEe)

                let (rf, rfe) =
                    match s0.forwardRate with
                    | Some (ReactionRate sf) -> (kf * sf |> ReactionRate |> Some, kfe * sf |> ReactionRate |> Some)
                    | None -> (None, None)

                (rf, rfe)
            | _ -> (None, None)

        {
            primary = { forwardRate = rf; backwardRate = None }
            similar = [ { reaction = re; rateData = { forwardRate = rfe; backwardRate = None } } ]
        }


    type AllRatesData =
        | FoodCreationRates of list<ReactionRateData<FoodCreationReaction>>
        | WasteRemovalRates of list<ReactionRateData<WasteRemovalReaction>>
        | WasteRecyclingRates of list<ReactionRateData<WasteRecyclingReaction>>
        | SynthesisRates of list<ReactionRateData<SynthesisReaction>>
        | SugarSynthesisRates of list<ReactionRateData<SugarSynthesisReaction>>
        | DestructionRates of list<ReactionRateData<DestructionReaction>>
        | CatalyticSynthesisRates of list<ReactionRateData<CatalyticSynthesisReaction>>
        | EnCatalyticSynthesisRates of list<ReactionRateData<EnCatalyticSynthesisReaction>>
        | CatalyticDestructionRates of list<ReactionRateData<CatalyticDestructionReaction>>
        | EnCatalyticDestructionRates of list<ReactionRateData<EnCatalyticDestructionReaction>>
        | LigationRates of list<ReactionRateData<LigationReaction>>
        | CatalyticLigationRates of list<ReactionRateData<CatalyticLigationReaction>>
        | EnCatalyticLigationRates of list<ReactionRateData<EnCatalyticLigationReaction>>
        | SedimentationDirectRates of list<ReactionRateData<SedimentationDirectReaction>>
        | SedimentationAllRates of list<ReactionRateData<SedimentationAllReaction>>
        | RacemizationRates of list<ReactionRateData<RacemizationReaction>>
        | CatalyticRacemizationRates of list<ReactionRateData<CatalyticRacemizationReaction>>
        | EnCatalyticRacemizationRates of list<ReactionRateData<EnCatalyticRacemizationReaction>>

        member ard.toReactionRates() =
            match ard with
            | FoodCreationRates r -> r |> List.map (fun e -> e.reaction |> FoodCreation, e.rateData)
            | WasteRemovalRates r -> r |> List.map (fun e -> e.reaction |> WasteRemoval, e.rateData)
            | WasteRecyclingRates r -> r |> List.map (fun e -> e.reaction |> WasteRecycling, e.rateData)
            | SynthesisRates r -> r |> List.map (fun e -> e.reaction |> Synthesis, e.rateData)
            | SugarSynthesisRates r -> r |> List.map (fun e -> e.reaction |> SugarSynthesis, e.rateData)
            | DestructionRates r -> r |> List.map (fun e -> e.reaction |> Destruction, e.rateData)
            | CatalyticSynthesisRates r -> r |> List.map (fun e -> e.reaction |> CatalyticSynthesis, e.rateData)
            | EnCatalyticSynthesisRates r -> r |> List.map (fun e -> e.reaction |> EnCatalyticSynthesis, e.rateData)
            | CatalyticDestructionRates r -> r |> List.map (fun e -> e.reaction |> CatalyticDestruction, e.rateData)
            | EnCatalyticDestructionRates r -> r |> List.map (fun e -> e.reaction |> EnCatalyticDestruction, e.rateData)
            | LigationRates r -> r |> List.map (fun e -> e.reaction |> Ligation, e.rateData)
            | CatalyticLigationRates r -> r |> List.map (fun e -> e.reaction |> CatalyticLigation, e.rateData)
            | EnCatalyticLigationRates r -> r |> List.map (fun e -> e.reaction |> EnCatalyticLigation, e.rateData)
            | SedimentationDirectRates r -> r |> List.map (fun e -> e.reaction |> SedimentationDirect, e.rateData)
            | SedimentationAllRates r -> r |> List.map (fun e -> e.reaction |> SedimentationAll, e.rateData)
            | RacemizationRates r -> r |> List.map (fun e -> e.reaction |> Racemization, e.rateData)
            | CatalyticRacemizationRates r -> r |> List.map (fun e -> e.reaction |> CatalyticRacemization, e.rateData)
            | EnCatalyticRacemizationRates r -> r |> List.map (fun e -> e.reaction |> EnCatalyticRacemization, e.rateData)


    type ReactionRateModelParam =
        | FoodCreationRateParam of FoodCreationParam
        | WasteRemovalRateParam of WasteRemovalParam
        | WasteRecyclingRateParam of WasteRecyclingParam
        | SynthesisRateParam of SynthesisParam
        | SugarSynthesisRateParam of SugarSynthesisParam
        | DestructionRateParam of DestructionParam
        | CatalyticSynthesisRateParam of CatalyticSynthesisParam
        | EnCatalyticSynthesisRateParam of EnCatalyticSynthesisParam
        | CatalyticDestructionRateParam of CatalyticDestructionParam
        | EnCatalyticDestructionRateParam of EnCatalyticDestructionParam
        | LigationRateParam of LigationParam
        | CatalyticLigationRateParam of CatalyticLigationParam
        | EnCatalyticLigationRateParam of EnCatalyticLigationParam
        | SedimentationDirectRateParam of SedimentationDirectParam
        | SedimentationAllRateParam of SedimentationAllParam
        | RacemizationRateParam of RacemizationParam
        | CatalyticRacemizationRateParam of CatalyticRacemizationParam
        | EnCatalyticRacemizationRateParam of EnCatalyticRacemizationParam


        /// TODO kk:20190317 - The dependencies MUST be incorporated at lower level so that to make it compiler's job to check them.
        /// Otherwise, if dependency is forgotten, it becomes hard to trace that bug.
        /// Essentially it must be made IMPOSSIBLE to write a code with the dependency and don't "declare" it upfront. Currently it is possible.
        member rm.dependsOn =
            match rm with
            | FoodCreationRateParam _ -> []
            | WasteRemovalRateParam _ -> []
            | WasteRecyclingRateParam _ -> []
            | SynthesisRateParam _ -> []
            | SugarSynthesisRateParam _ -> []
            | DestructionRateParam _ -> []
            | CatalyticSynthesisRateParam v ->
                match v with
                | CatSynthRndParam m -> [ m.synthesisParam |> SynthesisRateParam ]
                | CatSynthSimParam m -> [ m.catSynthParam |> CatSynthRndParam |> CatalyticSynthesisRateParam ]
            | EnCatalyticSynthesisRateParam v ->
                match v with
                | EnCatSynthRndParam m -> [ m.synthesisParam |> SynthesisRateParam ]
                | EnCatSynthSimParam m -> [ m.enCatSynthParam |> EnCatSynthRndParam |> EnCatalyticSynthesisRateParam ]
            | CatalyticDestructionRateParam v ->
                match v with
                | CatDestrRndParam m -> [ m.destructionParam |> DestructionRateParam ]
                | CatDestrSimParam m -> [ m.catDestrParam |> CatDestrRndParam |> CatalyticDestructionRateParam ]
            | EnCatalyticDestructionRateParam v ->
                match v with
                | EnCatDestrRndParam m -> [ m.destructionParam |> DestructionRateParam ]
                | EnCatDestrSimParam m -> [ m.enCatDestrParam |> EnCatDestrRndParam |> EnCatalyticDestructionRateParam ]
            | LigationRateParam _ -> []
            | CatalyticLigationRateParam v ->
                match v with
                | CatLigRndParam m -> [ m.ligationParam |> LigationRateParam ]
                | CatLigSimParam m -> [ m.catLigParam |> CatLigRndParam |> CatalyticLigationRateParam ]
            | EnCatalyticLigationRateParam v ->
                match v with
                | EnCatLigRndParam m -> [ m.ligationParam |> LigationRateParam ]
                | EnCatLigSimParam m -> [ m.enCatLigParam |> EnCatLigRndParam |> EnCatalyticLigationRateParam ]
            | SedimentationDirectRateParam v ->
                match v with
                | SedDirRndParam _ -> []
                | SedDirSimParam m -> [ m.sedDirParam |> SedDirRndParam |> SedimentationDirectRateParam ]
            | SedimentationAllRateParam _ -> []
            | RacemizationRateParam _ -> []
            | CatalyticRacemizationRateParam v ->
                match v with
                | CatRacemRndParam m -> [ m.racemizationParam |> RacemizationRateParam ]
                | CatRacemSimParam m -> [ m.catRacemParam |> CatRacemRndParam |> CatalyticRacemizationRateParam ]
            | EnCatalyticRacemizationRateParam v ->
                match v with
                | EnCatRacemRndParam m -> [ m.racemizationParam |> RacemizationRateParam ]
                | EnCatRacemSimParam m -> [ m.enCatRacemParam |> EnCatRacemRndParam |> EnCatalyticRacemizationRateParam ]


    type ReactionRateModelParamUsage =
        | PrimaryParam
        | DependsOnParam


    type ReactionRateModelParamWithUsage =
        {
            modelParam : ReactionRateModelParam
            usage : ReactionRateModelParamUsage
        }


    let tryPickParam picker (mp : list<ReactionRateModelParamWithUsage>) =
        let rec inner a b =
            match a with
            | [] -> None, b |> List.rev
            | h :: t ->
                match picker h with
                | Some x -> Some x, (b |> List.rev) @ t
                | None -> inner t (h :: b)

        inner mp []


    let rec allDep (rm : ReactionRateModelParam) (acc : list<ReactionRateModelParam>) =
        match rm.dependsOn with
        | [] -> acc
        | l -> l |> List.fold (fun a r -> allDep r (r :: a)) acc


    type ReactionRateProviderParams =
        {
            rateParams: list<ReactionRateModelParam>
            successNumberType : SuccessNumberType // kk:20191015 - currently there is one success number type for all relevant reactions.
        }

        member p.tryFindFoodCreationParam() = p.rateParams |> List.tryPick (fun e -> match e with | FoodCreationRateParam m -> Some m | _ -> None)
        member p.tryFindWasteRemovalParam() = p.rateParams |> List.tryPick (fun e -> match e with | WasteRemovalRateParam m -> Some m | _ -> None)
        member p.tryFindWasteRecyclingParam() = p.rateParams |> List.tryPick (fun e -> match e with | WasteRecyclingRateParam m -> Some m | _ -> None)
        member p.tryFindSynthesisParam() = p.rateParams |> List.tryPick (fun e -> match e with | SynthesisRateParam m -> Some m | _ -> None)
        member p.tryFindSugarSynthesisParam() = p.rateParams |> List.tryPick (fun e -> match e with | SugarSynthesisRateParam m -> Some m | _ -> None)
        member p.tryFindDestructionParam() = p.rateParams |> List.tryPick (fun e -> match e with | DestructionRateParam m -> Some m | _ -> None)
        member p.tryFindCatalyticSynthesisParam() = p.rateParams |> List.tryPick (fun e -> match e with | CatalyticSynthesisRateParam m -> Some m | _ -> None)
        member p.tryFindEnCatalyticSynthesisParam() = p.rateParams |> List.tryPick (fun e -> match e with | EnCatalyticSynthesisRateParam m -> Some m | _ -> None)
        member p.tryFindCatalyticDestructionParam() = p.rateParams |> List.tryPick (fun e -> match e with | CatalyticDestructionRateParam m -> Some m | _ -> None)
        member p.tryFindEnCatalyticDestructionParam() = p.rateParams |> List.tryPick (fun e -> match e with | EnCatalyticDestructionRateParam m -> Some m | _ -> None)
        member p.tryFindLigationParam() = p.rateParams |> List.tryPick (fun e -> match e with | LigationRateParam m -> Some m | _ -> None)
        member p.tryFindCatalyticLigationParam() = p.rateParams |> List.tryPick (fun e -> match e with | CatalyticLigationRateParam m -> Some m | _ -> None)
        member p.tryFindEnCatalyticLigationParam() = p.rateParams |> List.tryPick (fun e -> match e with | EnCatalyticLigationRateParam m -> Some m | _ -> None)
        member p.tryFindSedimentationDirectParam() = p.rateParams |> List.tryPick (fun e -> match e with | SedimentationDirectRateParam m -> Some m | _ -> None)
        member p.tryFindSedimentationAllParam() = p.rateParams |> List.tryPick (fun e -> match e with | SedimentationAllRateParam m -> Some m | _ -> None)
        member p.tryFindRacemizationParam() = p.rateParams |> List.tryPick (fun e -> match e with | RacemizationRateParam m -> Some m | _ -> None)
        member p.tryFindCatalyticRacemizationParam() = p.rateParams |> List.tryPick (fun e -> match e with | CatalyticRacemizationRateParam m -> Some m | _ -> None)
        member p.tryFindEnCatalyticRacemizationParam() = p.rateParams |> List.tryPick (fun e -> match e with | EnCatalyticRacemizationRateParam m -> Some m | _ -> None)

        member p.allParams() =
            let prim = p.rateParams |> Set.ofList
            let dep = Set.difference (p.rateParams |> List.map (fun e -> allDep e []) |> List.concat |> Set.ofList) prim

            (prim |> Set.map (fun e -> { modelParam = e; usage = PrimaryParam }))
            |> Set.union (dep |> Set.map (fun e -> { modelParam = e; usage = DependsOnParam }))
            |> Set.toList
            |> List.sort
