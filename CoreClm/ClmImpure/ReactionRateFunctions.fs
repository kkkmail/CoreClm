namespace ClmImpure

open System.Collections.Generic
open FSharp.Collections

open Clm.Distributions
open Clm.ReactionRatesBase
open Clm.ReactionRates
open Clm.ReactionRateParams
open Clm.ReactionTypes


module ReactionRateFunctions =

    let dictionaryToList (d : Dictionary<'R, (ReactionRate option * ReactionRate option)>) =
        d
        |> List.ofSeq
        |> List.map (fun e -> e.Key, e.Value)
        |> List.sortBy (fun (k, _) -> k)


    let updatePrimaryReactions<'R>
        (d : Dictionary<'R, RateData>)
        (getEnantiomer : 'R -> 'R)
        (primary : RateData)
        (r : 'R) =

        if d.Count > 0 && (d.Count % 1_000_000) = 0
        then printfn "updatePrimaryReactions::d.Count = %A for type: %A. Something is not right." d.Count (typedefof<'R>)

        let enantiomer = getEnantiomer r
        if d.ContainsKey r |> not then d.Add(r, primary)
        if d.ContainsKey enantiomer |> not then d.Add(enantiomer, primary)


    let updateSimilarReactions<'R>
        (d : Dictionary<'R, RateData>)
        (getEnantiomer : 'R -> 'R)
        (similar : list<ReactionRateData<'R>>) =

        similar |> List.map (fun e -> if d.ContainsKey e.reaction |> not then d.Add(e.reaction, e.rateData)) |> ignore
        similar |> List.map (fun e -> if d.ContainsKey (getEnantiomer e.reaction) |> not then d.Add(getEnantiomer e.reaction, e.rateData)) |> ignore


    let updateRelatedReactions<'R>
        (d : Dictionary<'R, RateData>)
        (getEnantiomer : 'R -> 'R)
        (r : 'R)
        (x : RelatedReactions<'R>) =

        updatePrimaryReactions d getEnantiomer x.primary r
        updateSimilarReactions d getEnantiomer x.similar
        x.primary


    let getRatesImpl<'R>
        (rateDictionary : Dictionary<'R, RateData>)
        (getEnantiomer : 'R -> 'R)
        (calculateRates : 'R -> RelatedReactions<'R>)
        (reaction : 'R)  =
        match rateDictionary.TryGetValue reaction with
        | true, rates -> rates
        | false, _ ->
            calculateRates reaction
            |> updateRelatedReactions rateDictionary getEnantiomer reaction


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
            rateDictionary : Dictionary<'RC, RateData>
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
        updateRelatedReactions i.rateDictionary i.getCatReactEnantiomer reaction related


    let getEeParams i cr cre rateMult d =
        match d with
        | true ->
            {
                rateMultiplierDistr = i.simParams.getRateMultiplierDistr.getDistr None rateMult
                eeDistribution = i.simParams.getEeDistr.getDistr cr.forwardRate cre.forwardRate
            }
        | false -> CatRatesEeParam.defaultValue


    let getRateMult br cr cre =
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


    let calculateSedDirSimRates (i : SedDirRatesSimInfo) =
        let r = (i.sedDirRatesInfo.sedFormingSubst, i.sedDirRatesInfo.sedDirAgent) |> SedimentationDirectReaction
        let re = (i.sedDirRatesInfo.sedFormingSubst, i.sedDirRatesInfo.sedDirAgent.enantiomer) |> SedimentationDirectReaction

        let cr = i.sedDirRatesInfo.getBaseRates r

        let calculateSedDirRates s c ee =
            let reaction = (s, c) |> SedimentationDirectReaction
            let related = calculateSedDirRates { i.sedDirRatesInfo with sedFormingSubst = s; sedDirAgent = c; eeParams = ee }
            updateRelatedReactions i.rateDictionary (fun e -> e.enantiomer) reaction related

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
            rateDictionary : Dictionary<'RCS, RateData>
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
        updateRelatedReactions i.rateDictionary i.getCatReactEnantiomer reaction related


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
        |> List.map (fun a -> creator a)
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
