namespace ClmImpure

open System.Collections.Generic
open FSharp.Collections

open Clm.Distributions
open Clm.Substances
open Clm.ReactionRatesBase
open Clm.ReactionRates
open ClmSys.DistributionData
open Clm.ReactionRateParams
open Clm.ReactionTypes

module ReactionRateFunctions =

    /// !!! Internally mutable structure !!!
    /// Structure to hold the key set and a function to produce a key out of reaction.
    type KeySetData<'R, 'C> =
        {
            keySet : HashSet<'C>
            getReactionKey : 'R -> 'C
        }


    /// !!! Internally mutable structure !!!
    type DictionaryData<'R, 'C> =
        {
            keySetData : KeySetData<'R, 'C> option
            rateDictionary : Dictionary<'R, RateData>
        }

        member d.tryGetReactionKey r = d.keySetData |> Option.bind (fun v -> v.getReactionKey r |> Some)

        member d.hasReactionKey r =
            let v =
                match d.keySetData with
                | Some data ->
                    let reactionKey = data.getReactionKey r
                    let retVal = data.keySet.Contains reactionKey
//                    printfn $"DictionaryData.hasReactionKey {r} = {retVal}, reactionKey = {reactionKey}, data.keySet.Count = {data.keySet.Count}."
                    retVal
                | None ->
                    false

//            printfn $"DictionaryData.hasReactionKey {r} = {v}."
            v


    let toDictionaryData r =
        {
            keySetData = None
            rateDictionary = r
        }


    type DictionaryData<'R> = DictionaryData<'R, 'R>


    let updateDictionary<'R, 'C>
        (d : DictionaryData<'R, 'C>)
        (data : RateData)
        (r : 'R) =

        let update() =
//            printfn $"updateDictionary.update r = {r}, data = {data}."
            if d.rateDictionary.ContainsKey r |> not then d.rateDictionary.Add(r, data)

        match d.keySetData with
        | None -> update()
        | Some v ->
            let reactionKey = v.getReactionKey r

            match v.keySet.Contains reactionKey with
            | false ->
//                printfn $"updateDictionary: reactionKey = {reactionKey}, r = {r}, data = {data}."
                update()
                v.keySet.Add reactionKey |> ignore
            | true ->
                match data.forwardRate, data. backwardRate with
                | Some _, Some _ -> update()
                | Some _, None -> update()
                | None, Some _ -> update()
                | None, None -> ()


    let dictionaryToList (d : Dictionary<'R, (ReactionRate option * ReactionRate option)>) =
        d
        |> List.ofSeq
        |> List.map (fun e -> e.Key, e.Value)
        |> List.sortBy (fun (k, _) -> k)


    let updatePrimaryReactions<'R, 'C>
        (d : DictionaryData<'R, 'C>)
        (getEnantiomer : 'R -> 'R)
        (primary : RateData)
        (r : 'R) =
        if d.rateDictionary.Count > 0 && (d.rateDictionary.Count % 1_000_000) = 0
        then printfn "updatePrimaryReactions::d.Count = %A for type: %A. Something is not right." d.rateDictionary.Count (typedefof<'R>)

        let enantiomer = getEnantiomer r
        updateDictionary d primary r
        updateDictionary d primary enantiomer


    let updateSimilarReactions<'R, 'C>
        (d : DictionaryData<'R, 'C>)
        (getEnantiomer : 'R -> 'R)
        (similar : list<ReactionRateData<'R>>) =
        similar |> List.map (fun e -> updateDictionary d e.rateData e.reaction) |> ignore
        similar |> List.map (fun e -> updateDictionary d e.rateData (getEnantiomer e.reaction)) |> ignore


    let updateRelatedReactions<'R, 'C>
        (d : DictionaryData<'R, 'C>)
        (getEnantiomer : 'R -> 'R)
        (r : 'R)
        (x : RelatedReactions<'R>) =

        updatePrimaryReactions d getEnantiomer x.primary r
        updateSimilarReactions d getEnantiomer x.similar
        x.primary


    type RelatedAcReactionsUpdateInfo<'RCA, 'CA, 'RA> =
        {
            dictionaryData : DictionaryData<'RCA, 'CA>
            acRateDictionary : Dictionary<'RA, RateData>
            getEnantiomer : 'RCA -> 'RCA
            getAcEnantiomer : 'RA -> 'RA
        }


    let updateRelatedAcReactions<'RCA, 'CA, 'RA>
        (i : RelatedAcReactionsUpdateInfo<'RCA, 'CA, 'RA>)
        (r : 'RCA)
        (x : RelatedAcReactions<'RCA, 'RA>) =

        updatePrimaryReactions i.dictionaryData i.getEnantiomer x.acPrimary r
        updateSimilarReactions i.dictionaryData i.getEnantiomer x.acSimilar

        let (d : DictionaryData<'RA, 'RA>) = toDictionaryData i.acRateDictionary

        x.activationData
        |> List.map (fun e -> updatePrimaryReactions d i.getAcEnantiomer e.rateData e.reaction)
        |> ignore

        x.acPrimary


    /// Samples a new value out of a given data array.
    /// If duplicates need to be excluded (ExcludeDuplicates) then
    /// excludes both found element and its enantiomer.
    let generateValue (d : Distribution) rnd (data : array<'A>) (getEnantiomer : 'A -> 'A) coll generated =
//        printfn "\n\ngenerateValue:Starting..."

        let getValue next =
//            printfn $"generateValue.getValue: next = {next}, data.Length = {data.Length}."
            let nextVal = data.[next]
            let e = getEnantiomer nextVal
            let g a = a |> List.distinct |> List.sort, nextVal
            let x = next :: generated

            match data |> Array.tryFindIndex (fun a -> a = e) with
            | None ->
//                printfn $"generateValue.getValue: Unable to find index for nextVal = '{nextVal}', e = '{e}'."
                g x
            | Some v ->
//                printfn $"generateValue.getValue: v = {v}."
                g (v :: x)

        let adjust next =
//            printfn "generateValue.adjust: next = %A" next

            let rec inner rem n =
//                printfn "generateValue.adjust.inner: n = %A" n
                match rem with
                | [] -> n
                | h :: t ->
                    match h > n with
                    | true -> n
                    | false -> inner t (n + 1)
            inner generated next

        match coll with
        | NoCollisionResolution ->
            let next = d.nextN rnd data.Length
//            printfn "generateValue: next = %A" next
            getValue next
        | ExcludeDuplicates ->
            let next = d.nextN rnd (data.Length - generated.Length)
            let adjusted = adjust next
//            printfn "generateValue: next = %A, adjusted = %A" next adjusted
            getValue adjusted


    /// Samples n unique values without collisions from an array.
    let generateUniqueValues (d : Distribution) rnd (data : array<'A>) n =
        let generate (idx, gen) =
            let (i, a) = generateValue d rnd data id ExcludeDuplicates idx
            (i, a :: gen)

        let (_, a) =
            [ for _ in 1..n -> () ]
            |> List.fold (fun e _ -> generate e) ([], [])

        a |> List.rev


    let getRatesImpl<'R, 'C>
        (d : DictionaryData<'R, 'C>)
        (getEnantiomer : 'R -> 'R)
        (calculateRates : 'R -> RelatedReactions<'R>)
        (reaction : 'R)  =
//        printfn $"getRatesImpl: reaction = {reaction}."
        let result =
            match d.rateDictionary.TryGetValue reaction with
            | true, rates -> rates
            | false, _ ->
                match d.hasReactionKey reaction with
                | false ->
                    calculateRates reaction
                    |> updateRelatedReactions d getEnantiomer reaction
                | true ->
                    // This is only applicable for a small number of reaction types where the number of reactions is extremely
                    // large, e.g. AcFwdCatalyticLigationReaction.
                    // If we end up here then the situation is as follows:
                    //     1. One of the previous reactions had the same reaction key (== catalyst).
                    //     2. However, DictionaryUpdateType.NonOptionalRateDataOnly was used and that resulted that we did
                    //        not store all the reactions but only a reaction key.
                    //     3. Had we stored the reaction in the dictionary, then it would've now returned a reaction with no data.
                    //     4. So, we need to do the same here.
                    {
                        forwardRate = None
                        backwardRate = None
                    }

//        match box reaction with
//        | :? ActivationReaction as r -> printfn $"getRatesImpl: reaction: {r}, result: %0A{result}."
//        | _ -> ()

        result

    let getAcRatesImpl<'RCA, 'CA, 'RA>
        (i : RelatedAcReactionsUpdateInfo<'RCA, 'CA, 'RA>)
        (calculateRates : 'RCA -> RelatedAcReactions<'RCA, 'RA>)
        (reaction : 'RCA)  =
//        printfn $"getAcRatesImpl: reaction = {reaction}."

        match i.dictionaryData.rateDictionary.TryGetValue reaction with
        | true, rates -> rates
        | false, _ ->
            match i.dictionaryData.hasReactionKey reaction with
            | false ->
                calculateRates reaction
                |> updateRelatedAcReactions i reaction
            | true ->
                // This is only applicable for a small number of reaction types where the number of reactions is extremely
                // large, e.g. AcFwdCatalyticLigationReaction.
                // If we end up here then the situation is as follows:
                //     1. One of the previous reactions had the same reaction key (== catalyst).
                //     2. However, DictionaryUpdateType.NonOptionalRateDataOnly was used and that resulted that we did
                //        not store all the reactions but only a reaction key.
                //     3. Had we stored the reaction in the dictionary, then it would've now returned a reaction with no data.
                //     4. So, we need to do the same here.
                {
                    forwardRate = None
                    backwardRate = None
                }


    //let inline getModelRates<'M, 'R when 'M : (member getRates : 'R -> (ReactionRate option * ReactionRate option))>
    //    (mo : 'M option) (r : 'R) : (ReactionRate option * ReactionRate option) =
    //    match mo with
    //    | Some m -> ((^M) : (member getRates : 'R -> (ReactionRate option * ReactionRate option)) (m, r))
    //    | None -> (None, None)
    //
    //
    //let inline getModelRates2<'M, 'R when 'M : (member getRates : RateGenerationType -> 'R -> (ReactionRate option * ReactionRate option))>
    //    (mo : 'M option) (t : RateGenerationType) (r : 'R) : (ReactionRate option * ReactionRate option) =
    //    match mo with
    //    | Some m -> ((^M) : (member getRates : RateGenerationType -> 'R -> (ReactionRate option * ReactionRate option)) (m, t, r))
    //    | None -> (None, None)


    type CatRatesSimInfo<'A, 'R, 'C, 'RC when 'R : equality> =
        {
            reaction : 'R
            catalyst : 'C

            /// Gets the list of reaction data "objects" to choose from.
            /// For e.g. catalytic synthesis this is a list of all amino acids.
            ///
            /// For catalytic ligation this may depend on the model.
            /// The most standard model returns list of all peptide bonds of the same symmetry.
            /// If input reaction's peptide bond is (A, B) and we have 3 amino acids (A, B, C) then
            /// the model will return [ (A, A); (A, B), (A, C); (B, A); (B, B); (B, C); (C, A); (C, B), (C, C) ]
            getReactionData : 'R -> list<'A>

            /// Produces the underlying data for a given reaction.
            inverse : 'R -> 'A

            /// Adjusts rate multiplier for matching reactions.
            getMatchingReactionMult : double -> double

            getCatEnantiomer : 'C -> 'C
            catReactionCreator : ('R * 'C) -> 'RC

            /// Creates reactions, which are similar to a given input.
            /// For synthesis this is just the reaction itself.
            /// For ligation, the most standard model returns all ligation reaction with the same peptide bond.
            /// E.g. if a bond is <P1>A + b<P2> -> <P1>Ab<P2> then the most standard model will return all ligation
            /// reactions which bind A and b in that order.
            simReactionCreator : 'A -> list<'R>

            getCatReactEnantiomer : 'RC -> 'RC

            /// Get rates of base (not catalyzed) reaction.
            getBaseRates : 'R -> RateData

            /// Get rates of underlying catalyzed reaction.
            getBaseCatRates : 'RC -> RateData

            simParams : CatRatesSimilarityParam
            eeParams : CatRatesEeParam
            dictionaryData : DictionaryData<'RC, 'C>
            rateGenerationType : RateGenerationType
            rnd : RandomValueGetter
        }

        member i.toCatRatesInfo r c e =
            {
                reaction = r
                catalyst = c
                getCatEnantiomer = i.getCatEnantiomer
                catReactionCreator = i.catReactionCreator
                getBaseRates = i.getBaseRates
                eeParams = e
                rateGenerationType = i.rateGenerationType
                rnd = i.rnd
            }


    let calculateSimCatRates i s c e =
        let reaction = (s, c) |> i.catReactionCreator
        let related = i.toCatRatesInfo s c e |> calculateCatRates
//        printfn "calculateSimCatRates: related = %A" related
        updateRelatedReactions i.dictionaryData i.getCatReactEnantiomer reaction related


    let getEeParams i cr cre rateMult d =
        match d with
        | true ->
            {
                rateMultiplierDistr = i.simParams.getRateMultiplierDistr.getDistr None rateMult
                eeDistribution = i.simParams.getEeDistr.getDistr cr.forwardRate cre.forwardRate
            }
        | false -> CatRatesEeParam.defaultValue


    let getRateMult br cr cre =
//        printfn $"getRateMult: br = {br}, cr = {cr}, cre = {cre}."
        match cr.forwardRate, cre.forwardRate, cr.backwardRate, cre.backwardRate with
        | Some (ReactionRate a), Some (ReactionRate b), _, _ ->
            match br.forwardRate with
            | Some (ReactionRate c) -> (a + b) / 2.0 / c
            | None -> failwith "calculateSimRates::calculateCatRates::FUBAR #1..."
        | _, _, Some (ReactionRate a), Some (ReactionRate b) ->
            match br.backwardRate with
            | Some (ReactionRate c) -> (a + b) / 2.0 / c
            | None -> failwith "calculateSimRates::calculateCatRates::FUBAR #2..."
        | _ -> failwith "calculateSimRates::calculateCatRates::FUBAR #3..."


    let getSimNoRates i creator aa r =
        aa
        |> List.map (fun a -> creator a)
        |> List.concat
        |> List.map (fun e -> e, calculateSimCatRates i e i.catalyst CatRatesEeParam.defaultValue)


    let chooseData i aa =
        let a =
            match i.simParams.catRatesSimGeneration with
            | DistributionBased simBaseDistribution -> aa |> List.map (fun a -> a, simBaseDistribution.isDefined i.rnd)
            | FixedValue d ->
                /// TODO kk:20200607 - Follow the description below.
                /// Here we need to ensure that number of successes is NOT random but fixed
                /// and that we always include the reactions with the same "data".
                /// This probably should change and be controlled by distributions (as most of the things here), but not today.
                let isDefined j x =
                    let b = (i.inverse (i.reaction)) = x

                    match b, d.value.distributionParams.threshold with
                    | true, _ -> true
                    | false, Some t -> (double j) < t * (double aa.Length)
                    | false, None -> true

                aa
                |> List.map(fun a -> i.rnd.nextDouble(), a)
                |> List.sortBy (fun (r, _) -> r)
                |> List.mapi (fun j (_, a) -> a, isDefined j a)

        a


    let getSimRates i aa getEeParams rateMult =
//        printfn "getSimRates: aa = %A\n" ("[ " + (aa |> List.fold (fun acc r -> acc + (if acc <> "" then "; " else "") + r.ToString()) "") + " ]")

        let x =
            chooseData i aa
            |> List.map (fun (e, b) -> e, b, match b with | true -> i.getMatchingReactionMult rateMult | false -> 0.0)

//        x
//        |> List.filter (fun (_, b, _) -> b)
//        |> List.sortBy (fun (a, _, _) -> a.ToString())
//        |> List.map (fun (a, _, r) -> printfn "x: a = %s, r = %A" (a.ToString()) r)
//        |> ignore
//        printfn "\n"

        let a =
            x
            |> List.map (fun (a, b, m) -> i.simReactionCreator a |> List.map (fun e -> e, b, m))
            |> List.concat

//        a
//        |> List.filter (fun (_, b, _) -> b)
//        |> List.sortBy (fun (a, _, _) -> a.ToString())
//        |> List.map (fun (a, _, r) -> printfn "a: a = %s, r = %A" (a.ToString()) r)
//        |> ignore
//        printfn "\n"

        let b =
            a
            |> List.filter (fun (e, _, _) -> e <> i.reaction)
            |> List.map (fun (e, b, m) -> e, calculateSimCatRates i e i.catalyst (getEeParams m b))

//        b
//        |> List.filter (fun (_, r) -> match (r.forwardRate, r.backwardRate) with | None, None -> false | _ -> true)
//        |> List.sortBy (fun (a, _) -> a.ToString())
//        |> List.map (fun (a, r) -> printfn "b: a = %s, r = %s" (a.ToString()) (r.ToString()))
//        |> ignore
//        printfn "\n"

        b


    let calculateSimRates<'A, 'R, 'C, 'RC when 'A : equality and 'R : equality> (i : CatRatesSimInfo<'A, 'R, 'C, 'RC>) =
        let r = (i.reaction, i.catalyst) |> i.catReactionCreator
        let re = (i.reaction, i.getCatEnantiomer i.catalyst) |> i.catReactionCreator
        let br = i.getBaseRates i.reaction // (bf, bb)
        let cr = r |> i.getBaseCatRates // (f, b)
        let aa = i.getReactionData i.reaction

//        printfn "calculateSimRates: r = %s\n\n" (r.ToString())

        match (cr.forwardRate, cr.backwardRate) with
        | None, None -> getSimNoRates i i.simReactionCreator aa i.reaction
        | _ ->
            let cre = re |> i.getBaseCatRates
            let rateMult = getRateMult br cr cre
//            printfn "calculateSimRates: br = %s, cr = %s, cre = %s, rateMult = %A\n" (br.ToString()) (cr.ToString()) (cre.ToString()) rateMult
            let getEeParams = getEeParams i cr cre
            getSimRates i aa getEeParams rateMult
        |> ignore

        cr


    let calculateActivationRates (i : ActivationRandomInfo) (r : ActivationReaction) =
        let (ActivationReaction (s, p)) = r
        let re = (s.enantiomer, p) |> ActivationReaction

        let rf, rfe =
            let k0 = i.activationParam.activationDistribution.nextDouble i.rnd

            match i.activationParam.eeDistribution with
            | Some df ->
                let fEe = df.nextDouble i.rnd

                let kf = k0 * (1.0 + fEe)
                let kfe = k0 * (1.0 - fEe)
                let (rf, rfe) = kf |> ReactionRate |> Some, kfe |> ReactionRate |> Some
                (rf, rfe)
            | _ -> (None, None)

        {
            primary = { forwardRate = rf; backwardRate = None }
            similar = [ { reaction = re; rateData = { forwardRate = rfe; backwardRate = None } } ]
        }


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


    type SedDirRatesSimInfo =
        {
            sedDirRatesInfo : SedDirRatesInfo
            aminoAcids : list<AminoAcid>
            reagents : Map<AminoAcid, list<SedDirReagent>>
            simParams : SedDirSimilarityParam
            dictionaryData : DictionaryData<SedimentationDirectReaction>
        }


    let calculateSedDirSimRates (i : SedDirRatesSimInfo) =
        let r = (i.sedDirRatesInfo.sedFormingSubst, i.sedDirRatesInfo.sedDirAgent) |> SedimentationDirectReaction
        let re = (i.sedDirRatesInfo.sedFormingSubst, i.sedDirRatesInfo.sedDirAgent.enantiomer) |> SedimentationDirectReaction

        let cr = i.sedDirRatesInfo.getBaseRates r

        let calculateSedDirRates s c ee =
            let reaction = (s, c) |> SedimentationDirectReaction
            let related = calculateSedDirRates { i.sedDirRatesInfo with sedFormingSubst = s; sedDirAgent = c; eeParams = ee }
            updateRelatedReactions i.dictionaryData (fun e -> e.enantiomer) reaction related

        match cr.forwardRate with
        | None ->
            i.aminoAcids
            |> List.map (fun a -> i.reagents.[a])
            |> List.concat
            |> List.map (fun e -> calculateSedDirRates e i.sedDirRatesInfo.sedDirAgent SedDirRatesEeParam.defaultValue)
            |> ignore
        | Some (ReactionRate a) ->
            let cre = re |> i.sedDirRatesInfo.getBaseRates

            let m =
                match i.sedDirRatesInfo.eeParams.sedDirRateMultiplierDistr.value with
                | Some v -> v.mean
                | None -> 1.0

            let rateMult =
                match cre.forwardRate with
                | Some (ReactionRate b) ->(a + b) / 2.0 / m
                | _ -> failwith "calculateSedDirSimRates::calculateCatRates::FUBAR #1..."

            let getEeParams d =
                match d with
                | true ->
                    {
                        sedDirRateMultiplierDistr = i.simParams.getRateMultiplierDistr.getDistr None rateMult
                        eeDistribution = i.simParams.getEeDistr.getDistr cr.forwardRate cre.forwardRate
                    }
                | false -> SedDirRatesEeParam.defaultValue

            i.aminoAcids
            |> List.map (fun a -> i.reagents.[a], i.simParams.sedDirSimBaseDistribution.isDefined i.sedDirRatesInfo.rnd)
            |> List.map (fun (e, b) -> e |> List.map (fun a -> (a, b)))
            |> List.concat
            |> List.map (fun (e, b) -> calculateSedDirRates e i.sedDirRatesInfo.sedDirAgent (getEeParams b))
            |> ignore

        cr


    let getAllRatesImpl (d : Dictionary<'R, RateData>) =
        d
        |> Seq.map (|KeyValue|)
        |> List.ofSeq
        |> List.map (fun (r, d) -> { reaction = r; rateData = d })


    /// Similar to CatRatesSimInfo but for the reactions with energy consumption.
    type EnCatRatesSimInfo<'A, 'R, 'C, 'S, 'RCS when 'R : equality> =
        {
            reaction : 'R
            enCatalyst : 'C
            energySource : 'S
            getReactionData : 'R -> list<'A>
            inverse : 'R -> 'A
            getMatchingReactionMult : double -> double
            getCatEnantiomer : 'C -> 'C
            getEnergySourceEnantiomer : 'S -> 'S
            enCatReactionCreator : ('R * 'C * 'S) -> 'RCS
            simReactionCreator : 'A -> list<'R>
            getCatReactEnantiomer : 'RCS -> 'RCS
            getBaseRates : 'R -> RateData
            getBaseCatRates : 'RCS -> RateData
            enSimParams : EnCatRatesSimilarityParam
            eeParams : EnCatRatesEeParam
            dictionaryData : DictionaryData<'RCS, ('C * 'S)>
            rateGenerationType : RateGenerationType
            rnd : RandomValueGetter
        }

        member i.toEnCatRatesInfo r c e =
            {
                reaction = r
                enCatalyst = c
                energySource = i.energySource
                getCatEnantiomer = i.getCatEnantiomer
                getEnergySourceEnantiomer = i.getEnergySourceEnantiomer
                enCatReactionCreator = i.enCatReactionCreator
                getBaseRates = i.getBaseRates
                eeParams = e
                rateGenerationType = i.rateGenerationType
                rnd = i.rnd
            }


    let calculateSimEnCatRates i s c e =
        let reaction = (s, c, i.energySource) |> i.enCatReactionCreator
        let related = i.toEnCatRatesInfo s c e |> calculateEnCatRates
//        printfn "calculateSimEnCatRates: related = %A" related
        updateRelatedReactions i.dictionaryData i.getCatReactEnantiomer reaction related


    let getEnEeParams i cr cre rateMult d =
        match d with
        | true ->
            {
                rateMultiplierDistr = i.enSimParams.getRateMultiplierDistr.getDistr None rateMult
                enEeDistribution = i.enSimParams.getForwardEeDistr.getDistr cr.forwardRate cre.forwardRate
            }
        | false -> EnCatRatesEeParam.defaultValue


    let getEnSimNoRates i creator aa r =
        aa
        |> List.map creator
        |> List.concat
        |> List.map (fun e -> e, calculateSimEnCatRates i e i.enCatalyst EnCatRatesEeParam.defaultValue)


    /// Note that it is nearly identical to chooseData above.
    /// We keep them separately as it is likely that this function will be changed.
    let chooseEnData i aa =
        let a =
            match i.enSimParams.enCatRatesSimGeneration with
            | DistributionBased simBaseDistribution -> aa |> List.map (fun a -> a, simBaseDistribution.isDefined i.rnd)
            | FixedValue d ->
                /// TODO kk:20200607 - Follow the description below.
                /// Here we need to ensure that number of successes is NOT random but fixed
                /// and that we always include the reactions with the same "data".
                /// This probably should change and be controlled by distributions (as most of the things here), but not today.
                let isDefined j x =
                    let b = (i.inverse (i.reaction)) = x

                    match b, d.value.distributionParams.threshold with
                    | true, _ -> true
                    | false, Some t -> (double j) < t * (double aa.Length)
                    | false, None -> true

                aa
                |> List.map(fun a -> i.rnd.nextDouble(), a)
                |> List.sortBy (fun (r, _) -> r)
                |> List.mapi (fun j (_, a) -> a, isDefined j a)

        a

    let getEnSimRates i aa getEeParams rateMult =
//        printfn "getEnSimRates: aa = %A\n" ("[ " + (aa |> List.fold (fun acc r -> acc + (if acc <> "" then "; " else "") + r.ToString()) "") + " ]")

        let x =
            chooseEnData i aa
            |> List.map (fun (e, b) -> e, b, match b with | true -> i.getMatchingReactionMult rateMult | false -> 0.0)

//        x
//        |> List.filter (fun (_, b, _) -> b)
//        |> List.sortBy (fun (a, _, _) -> a.ToString())
//        |> List.map (fun (a, _, r) -> printfn "x: a = %s, r = %A" (a.ToString()) r)
//        |> ignore
//        printfn "\n"

        let a =
            x
            |> List.map (fun (a, b, m) -> i.simReactionCreator a |> List.map (fun e -> e, b, m))
            |> List.concat

//        a
//        |> List.filter (fun (_, b, _) -> b)
//        |> List.sortBy (fun (a, _, _) -> a.ToString())
//        |> List.map (fun (a, _, r) -> printfn "a: a = %s, r = %A" (a.ToString()) r)
//        |> ignore
//        printfn "\n"

        let b =
            a
            |> List.filter (fun (e, _, _) -> e <> i.reaction)
            |> List.map (fun (e, b, m) -> e, calculateSimEnCatRates i e i.enCatalyst (getEeParams m b))

//        b
//        |> List.filter (fun (_, r) -> match (r.forwardRate, r.backwardRate) with | None, None -> false | _ -> true)
//        |> List.sortBy (fun (a, _) -> a.ToString())
//        |> List.map (fun (a, r) -> printfn "b: a = %s, r = %s" (a.ToString()) (r.ToString()))
//        |> ignore
//        printfn "\n"

        b


    let calculateEnSimRates i =
        let r = (i.reaction, i.enCatalyst, i.energySource) |> i.enCatReactionCreator
        let re = (i.reaction, i.getCatEnantiomer i.enCatalyst, i.energySource) |> i.enCatReactionCreator
        let ru = (i.reaction, i.enCatalyst, i.getEnergySourceEnantiomer i.energySource) |> i.enCatReactionCreator
        let reu = (i.reaction, i.getCatEnantiomer i.enCatalyst, i.getEnergySourceEnantiomer i.energySource) |> i.enCatReactionCreator

        let br = i.getBaseRates i.reaction // (bf, bb)
        let cr = r |> i.getBaseCatRates // (f, b)
        let aa = i.getReactionData i.reaction

//        printfn "calculateEnSimRates: r = %s\n\n" (r.ToString())

        match (cr.forwardRate, cr.backwardRate) with
        | None, None -> getEnSimNoRates i i.simReactionCreator aa i.reaction
        | _ ->
            let cre = re |> i.getBaseCatRates
            let rateMult = getRateMult br cr cre
//            printfn "calculateEnSimRates: br = %s, cr = %s, cre = %s, rateMult = %A\n" (br.ToString()) (cr.ToString()) (cre.ToString()) rateMult
            let getEnEeParams = getEnEeParams i cr cre
            getEnSimRates i aa getEnEeParams rateMult
        |> ignore

        cr

    /// 'A - type of "similarity" variable - e.g. amino acid for AC catalytic synthesis reaction or
    ///      a type of peptide bond symmetry for AC ligation reactions.
    /// 'R - base reaction
    /// 'CA - activated catalyst (C*)
    /// 'C - deactivated catalyst (C)
    /// 'RCA - reaction catalysed by activated catalyst, e.g. A + C* -> B + C
    /// 'RA - activation reaction e.g.: C + Z -> C* + W
    type AcCatRatesSimInfoProxy<'A, 'R, 'CA, 'C, 'RCA, 'RA when 'R : equality> =
        {
            acCatRatesInfoProxy : AcCatRatesInfoProxy<'R, 'CA, 'C, 'RCA, 'RA>
            inverse : 'R -> 'A
            getReactionData : 'R -> list<'A>
            simReactionCreator : 'A -> list<'R>
            getCatReactEnantiomer : 'RCA -> 'RCA
            getBaseCatRates : RandomValueGetter -> 'RCA -> RateData
            getMatchingReactionMult : double -> double
            tryGetBaseCatRates : 'RCA -> RateData
        }

    /// 'A - type of "similarity" variable - e.g. amino acid for AC catalytic synthesis reaction or
    ///      a type of peptide bond symmetry for AC ligation reactions.
    /// 'R - base reaction
    /// 'CA - activated catalyst (C*)
    /// 'C - deactivated catalyst (C)
    /// 'RCA - reaction catalysed by activated catalyst, e.g. A + C* -> B + C
    /// 'RA - activation reaction e.g.: C + Z -> C* + W
    type AcCatRatesSimInfo<'A, 'R, 'CA, 'C, 'RCA, 'RA when 'R : equality> =
        {
            reaction : 'R
            acCatalyst : 'CA
            acSimParams : AcCatRatesSimilarityParam
            acEeParams : AcCatRatesEeParam
            dictionaryData : DictionaryData<'RCA, 'CA>
            acRateDictionary : Dictionary<'RA, RateData>
            proxy : AcCatRatesSimInfoProxy<'A, 'R, 'CA, 'C, 'RCA, 'RA>

//            getNonActivated : 'CA -> 'C
//            getCatEnantiomer : 'CA -> 'CA
//            acCatReactionCreator : ('R * 'CA) -> 'RCA
//            activationReactorCreator : 'C -> 'RA
//            getBaseRates : 'R -> RateData
//            getActivationRates : 'RA -> RateData
//            rateGenerationType : RateGenerationType
//            rnd : RandomValueGetter

//            getCatReactEnantiomer : 'RCA -> 'RCA
        }

        member i.toAcCatRatesInfo r c e =
            {
                reaction = r
                acCatalyst = c
                acEeParams = e
                proxy = i.proxy.acCatRatesInfoProxy

//                getNonActivated = i.getNonActivated
//                getCatEnantiomer = i.getCatEnantiomer
//                acCatReactionCreator = i.acCatReactionCreator
//                activationReactorCreator = i.activationReactorCreator
//                getBaseRates = i.getBaseRates
//                getActivationRates = i.getActivationRates
//                rateGenerationType = i.rateGenerationType
//                rnd = i.rnd
            }



//    let updateRelatedAcReactions<'RCA, 'CA, 'RA>
//        (i : RelatedAcReactionsUpdateInfo<'RCA, 'CA, 'RA>)
//        (r : 'RCA)
//        (x : RelatedAcReactions<'RCA, 'RA>) =

    let calculateAcSimCatRates<'A, 'R, 'CA, 'C, 'RCA, 'RA when 'A : equality and 'R : equality> (i : AcCatRatesSimInfo<'A, 'R, 'CA, 'C, 'RCA, 'RA>) s c e =
        let reaction = (s, c) |> i.proxy.acCatRatesInfoProxy.acCatReactionCreator
        let related = i.toAcCatRatesInfo s c e |> calculateAcCatRates

        let (info : RelatedAcReactionsUpdateInfo<'RCA, 'CA, 'RA>) =
            {
                dictionaryData = i.dictionaryData // : DictionaryData<'RCA, 'CA>
                acRateDictionary = i.acRateDictionary //  : Dictionary<'RA, RateData>
                getEnantiomer = i.proxy.getCatReactEnantiomer //  : 'RCA -> 'RCA
                getAcEnantiomer = i.proxy.acCatRatesInfoProxy.getAcEnantiomer //  : 'RA -> 'RA
            }

//        printfn "calculateSimCatRates: related = %A" related
        updateRelatedAcReactions info reaction related


    let getAcEeParams i cr cre rateMult d  =
//        match d with
//        | AcNone -> AcCatRatesEeParam.defaultValue
//        | AcForwardRateOnly ->
//            {
//                rateMultiplierDistr = i.acSimParams.getRateMultiplierDistr.getDistr None rateMult
//                acFwdEeDistribution = i.acSimParams.getForwardEeDistr.getDistr cr.forwardRate cre.forwardRate
//            }
//            |> AcForwardRateOnlyParam
//        | AcBackwardRateOnly ->
//            {
//                rateMultiplierDistr = i.acSimParams.getRateMultiplierDistr.getDistr None rateMult
//                acBkwEeDistribution = i.acSimParams.getBackwardEeDistr.getDistr cr.backwardRate cre.backwardRate
//            }
//            |> AcBackwardRateOnlyParam

        match d with
        | true ->
            match i.acEeParams with
            | AcNoneParam _ -> AcCatRatesEeParam.defaultValue
            | AcForwardRateOnlyParam _ ->
                {
                    rateMultiplierDistr = i.acSimParams.getRateMultiplierDistr.getDistr None rateMult
                    acFwdEeDistribution = i.acSimParams.getForwardEeDistr.getDistr cr.forwardRate cre.forwardRate
                }
                |> AcForwardRateOnlyParam
            | AcBackwardRateOnlyParam _ ->
                {
                    rateMultiplierDistr = i.acSimParams.getRateMultiplierDistr.getDistr None rateMult
                    acBkwEeDistribution = i.acSimParams.getBackwardEeDistr.getDistr cr.backwardRate cre.backwardRate
                }
                |> AcBackwardRateOnlyParam
        | false -> AcCatRatesEeParam.defaultValue


//    let getRateMult br cr cre =
//        match cr.forwardRate, cre.forwardRate, cr.backwardRate, cre.backwardRate with
//        | Some (ReactionRate a), Some (ReactionRate b), _, _ ->
//            match br.forwardRate with
//            | Some (ReactionRate c) -> (a + b) / 2.0 / c
//            | None -> failwith "calculateSimRates::calculateCatRates::FUBAR #1..."
//        | _, _, Some (ReactionRate a), Some (ReactionRate b) ->
//            match br.backwardRate with
//            | Some (ReactionRate c) -> (a + b) / 2.0 / c
//            | None -> failwith "calculateSimRates::calculateCatRates::FUBAR #2..."
//        | _ -> failwith "calculateSimRates::calculateCatRates::FUBAR #3..."


    let getAcSimNoRates i creator aa r =
        aa
        |> List.map creator
        |> List.concat
        |> List.map (fun e -> e, calculateAcSimCatRates i e i.acCatalyst AcCatRatesEeParam.defaultValue)


    // (i : AcCatRatesSimInfo<'A, 'A, 'C, 'D>)
    let chooseAcData i aa =
        let a =
            let rnd = i.proxy.acCatRatesInfoProxy.rnd

            match i.acSimParams.acCatRatesSimGeneration with
            | DistributionBased simBaseDistribution -> aa |> List.map (fun a -> a, simBaseDistribution.isDefined rnd)
            | FixedValue d ->
                /// TODO kk:20200607 - Follow the description below.
                /// Here we need to ensure that number of successes is NOT random but fixed
                /// and that we always include the reactions with the same "data".
                /// This probably should change and be controlled by distributions (as most of the things here), but not today.
                let isDefined j x =
                    let b = (i.proxy.inverse (i.reaction)) = x

                    match b, d.value.distributionParams.threshold with
                    | true, _ -> true
                    | false, Some t -> (double j) < t * (double aa.Length)
                    | false, None -> true

                aa
                |> List.map(fun a -> rnd.nextDouble(), a)
                |> List.sortBy (fun (r, _) -> r)
                |> List.mapi (fun j (_, a) -> a, isDefined j a)

        a

    let getAcSimRates<'A, 'R, 'CA, 'C, 'RCA, 'RA when 'A : equality and 'R : equality> (i : AcCatRatesSimInfo<'A, 'R, 'CA, 'C, 'RCA, 'RA>) aa getEeParams rateMult =
        printfn "getAcSimRates: aa = %A\n" ("[ " + (aa |> List.fold (fun acc r -> acc + (if acc <> "" then "; " else "") + r.ToString()) "") + " ]")

        let x =
            chooseAcData i aa
            |> List.map (fun (e, b) -> e, b, match b with | true -> i.proxy.getMatchingReactionMult rateMult | false -> 0.0)

        x
        |> List.filter (fun (_, b, _) -> b)
        |> List.sortBy (fun (a, _, _) -> a.ToString())
        |> List.map (fun (a, _, r) -> printfn "x: a = %s, r = %A" (a.ToString()) r)
        |> ignore
        printfn "\n"

        let a =
            x
            |> List.map (fun (a, b, m) -> i.proxy.simReactionCreator a |> List.map (fun e -> e, b, m))
            |> List.concat

//        a
//        |> List.filter (fun (_, b, _) -> b)
//        |> List.sortBy (fun (a, _, _) -> a.ToString())
//        |> List.map (fun (a, _, r) -> printfn "a: a = %s, r = %A" (a.ToString()) r)
//        |> ignore
//        printfn "\n"

        let b =
            a
            |> List.filter (fun (e, _, _) -> e <> i.reaction)
            |> List.map (fun (e, b, m) -> e, calculateAcSimCatRates i e i.acCatalyst (getEeParams m b))

//        b
//        |> List.filter (fun (_, r) -> match (r.forwardRate, r.backwardRate) with | None, None -> false | _ -> true)
//        |> List.sortBy (fun (a, _) -> a.ToString())
//        |> List.map (fun (a, r) -> printfn "b: a = %s, r = %s" (a.ToString()) (r.ToString()))
//        |> ignore
//        printfn "\n"

        b


    let calculateAcSimRates<'A, 'R, 'CA, 'C, 'RCA, 'RA when 'A : equality and 'R : equality> (i : AcCatRatesSimInfo<'A, 'R, 'CA, 'C, 'RCA, 'RA>) =
        printfn $"calculateAcSimRates: Starting. i = {i}"
        let r = (i.reaction, i.acCatalyst) |> i.proxy.acCatRatesInfoProxy.acCatReactionCreator
        let re = (i.reaction, i.proxy.acCatRatesInfoProxy.getCatEnantiomer i.acCatalyst) |> i.proxy.acCatRatesInfoProxy.acCatReactionCreator
        let br = i.proxy.acCatRatesInfoProxy.getBaseRates i.reaction // (bf, bb)
        let aa = i.proxy.getReactionData i.reaction
        let rnd = i.proxy.acCatRatesInfoProxy.rnd

        printfn $"calculateAcSimRates: r = {r}, re = {re}"
        printfn "calculateAcSimRates: aa ="
        aa |> List.map (fun a -> printfn $"    {a}") |> ignore

        let cr =
            match i.dictionaryData.hasReactionKey r with
            | false ->
                printfn "calculateAcSimRates: Calling i.proxy.getBaseCatRates rnd r ..."
                i.proxy.getBaseCatRates rnd r
            | true ->
                printfn "calculateAcSimRates: reaction has a key (catalyst) in the dictionary. Do not create new reactions."
                i.proxy.tryGetBaseCatRates r

        printfn "calculateAcSimRates: cr = %A" cr

        match (cr.forwardRate, cr.backwardRate) with
        | None, None ->
            printfn "calculateAcSimRates: Calling getAcSimNoRates ..."
            getAcSimNoRates i i.proxy.simReactionCreator aa i.reaction
        | _ ->
            printfn "calculateAcSimRates: Getting cre ..."
            let cre = i.proxy.getBaseCatRates rnd re
            let rateMult = getRateMult br cr cre
            printfn $"calculateAcSimRates: br = {br}, cr = {cr}, cre = {cre}, rateMult = %A{rateMult}\n"
            let getAcEeParams = getAcEeParams i cr cre
            getAcSimRates i aa getAcEeParams rateMult
        |> ignore

        cr
