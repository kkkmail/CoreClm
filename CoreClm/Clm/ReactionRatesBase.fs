namespace Clm

open FSharp.Collections
open Clm.Distributions
open Clm.ReactionTypes
open ClmSys.ModelData

module ReactionRatesBase =

    /// Specifies how to generate rates.
    /// RandomChoice first randomly determine the reactions with non-zero rates and then gets these rates (without using threshold).
    type RateGenerationType =
        | RandomChoice


    type RateMult =
        {
            kf : double
            kfe : double
            kb : double
            kbe : double
        }


    /// Specifies how to apply similarity.
    /// DistrBased uses distribution threshold to determine if an amino acid should / should not be included.
    /// This results in some spread in the number of amino acids.
    /// FixedVal - fixes the number of amino acids, but the choice of them is still random.
    type CatRatesSimGenType =
        | DistrBased
        | FixedVal


    /// Specifies how to generate catalytic rates.
    /// ByIndividualCatalyst - enforces thermodynamic constraint on each catalyst.
    /// ByEnantiomerPairs - enforces thermodynamic constraint on a pair of enantiomer catalysts.
    type CatalyticRateGenerationType =
        | ByIndividualCatalyst of CatRatesSimGenType
        | ByEnantiomerPairs of CatRatesSimGenType

        member this.catRatesSimGenType = match this with | ByIndividualCatalyst c | ByEnantiomerPairs c -> c


    type RateData =
        {
            forwardRate : ReactionRate option
            backwardRate : ReactionRate option
        }

        override r.ToString() = $"{{ f: %A{r.forwardRate}; b: %A{r.backwardRate} }}"

        static member defaultValue =
            {
                forwardRate = None
                backwardRate = None
            }


    let bind f xOpt =
        match xOpt with
        | Some x -> f x
        | _ -> { forwardRate = None; backwardRate = None }


    type ReactionRateData<'R> =
        {
            reaction : 'R
            rateData : RateData
        }


    type RateGeneratorInfo<'A, 'B> =
        {
            pairCollision : PairCollisionResolution
            a : array<'A>
            b : array<'B>
            getEnantiomerA : 'A -> 'A
            getEnantiomerB : 'B -> 'B
            reactionName : ReactionName
            successNumberType : SuccessNumberType
        }


    type RateGeneratorInfo<'A, 'B, 'C> =
        {
            tripleCollision : TripleCollisionResolution
            a : array<'A>
            b : array<'B>
            c : array<'C>
            getEnantiomerA : 'A -> 'A
            getEnantiomerB : 'B -> 'B
            getEnantiomerC : 'C -> 'C
            reactionName : ReactionName
            successNumberType : SuccessNumberType
        }


    type RelatedReactions<'RC> =
        {
            primary : RateData
            similar : list<ReactionRateData<'RC>>
        }


    type RelatedAcReactions<'RCA, 'RA> =
        {
            acPrimary : RateData
            acSimilar : list<ReactionRateData<'RCA>>
            activationData : list<ReactionRateData<'RA>>
        }


    let getRatesWithSimilar (fo, rf) (bo, rb) s =
        let g so ro =
            match so, ro with
            | Some s, Some r -> s * r |> ReactionRate |> Some
            | _ -> None

        {
            primary = { forwardRate = g fo rf; backwardRate = g bo rb }
            similar = s
        }


    let getRates (fo, rf) (bo, rb) = getRatesWithSimilar (fo, rf) (bo, rb) []
    let getForwardRates (fo, rf) = getRates (fo, rf) (None, None)


    type CatRatesEeParam =
        {
            rateMultiplierDistr : RateMultiplierDistribution
            eeDistribution : EeDistribution option
        }

        static member defaultValue =
            {
                rateMultiplierDistr = NoneRateMult
                eeDistribution = None
            }


    type CatRatesInfo<'R, 'C, 'RC> =
        {
            reaction : 'R
            catalyst : 'C
            getCatEnantiomer : 'C -> 'C
            catReactionCreator : ('R * 'C) -> 'RC
            getBaseRates : 'R -> RateData // Get rates of base (not catalyzed) reaction.
            eeParams : CatRatesEeParam
            rateGenerationType : RateGenerationType
            rnd : RandomValueGetter
        }


    /// Thermodynamic considerations require that the equilibrium does not change in the presence of catalyst.
    let calculateCatRates<'R, 'C, 'RC> (i : CatRatesInfo<'R, 'C, 'RC>) =
        let re = (i.reaction, i.getCatEnantiomer i.catalyst) |> i.catReactionCreator

        let rf, rb, rfe, rbe =
            let k =
                match i.rateGenerationType with
                | RandomChoice -> i.eeParams.rateMultiplierDistr.nextDouble i.rnd

            match k, i.eeParams.eeDistribution with
            | Some k0, Some df ->
                let s0 = i.getBaseRates i.reaction
                let fEe = df.nextDouble i.rnd

                let kf = k0 * (1.0 + fEe)
                let kfe = k0 * (1.0 - fEe)

                let (rf, rfe) =
                    match s0.forwardRate with
                    | Some (ReactionRate sf) -> (kf * sf |> ReactionRate |> Some, kfe * sf |> ReactionRate |> Some)
                    | None -> (None, None)

                let (rb, rbe) =
                    match s0.backwardRate with
                    | Some (ReactionRate sb) -> (kf * sb |> ReactionRate |> Some, kfe * sb |> ReactionRate |> Some)
                    | None -> (None, None)

                (rf, rb, rfe, rbe)
            | _ -> (None, None, None, None)

        {
            primary = { forwardRate = rf; backwardRate = rb }
            similar = [ { reaction = re; rateData = { forwardRate = rfe; backwardRate = rbe } } ]
        }


    type CatRatesSimGeneration =
        | DistributionBased of Distribution
        | FixedValue of Distribution


    type CatRatesSimilarityParam =
        {
            catRatesSimGeneration : CatRatesSimGeneration
            getRateMultiplierDistr : RateMultiplierDistributionGetter
            getEeDistr : EeDistributionGetter
        }


    type EnCatRatesEeParam =
        {
            rateMultiplierDistr : RateMultiplierDistribution
            enEeDistribution : EeDistribution option
        }

        static member defaultValue =
            {
                rateMultiplierDistr = NoneRateMult
                enEeDistribution = None
            }


    type EnCatRatesInfo<'R, 'C, 'S, 'RCS> =
        {
            reaction : 'R
            enCatalyst : 'C
            energySource : 'S
            getCatEnantiomer : 'C -> 'C
            getEnergySourceEnantiomer : 'S -> 'S
            enCatReactionCreator : ('R * 'C * 'S) -> 'RCS
            getBaseRates : 'R -> RateData // Get rates of base (not catalyzed) reaction.
            eeParams : EnCatRatesEeParam
            rateGenerationType : RateGenerationType
            rnd : RandomValueGetter
        }

    /// TODO kk:20201129 - The logic of getting random values of f, fe, fu, and feu seems wrong.
    /// The reaction goes as follows [REF]:
    ///     1. The "energy source" molecule is consumed and it activates the "catalyst".
    ///     2. That means that the activation part of kf: (R, C, U) -> (C, U) must be the same as of
    ///        kfeu: (R, E(C), E(U)) -> (E(C), E(U)) == E (C, U) and similarly for kfe vs kfu.
    ///     3. Once the activation happens (and provided that the chiral remains of the energy source molecule is
    ///        not attached to the catalyst anymore), then the chiral part of the energy source is no longer available.
    ///     4. Which means that the activated catalyst now can "utilize" its enantioselectivity.
    ///     5. Current code DOES NOT follow this symmetry.
    ///
    /// These reactions explicitly consume energy by utilizing "energy source" molecule, which is destroyed.
    /// Subsequently, thermodynamic equilibrium IS changed as a result of such reaction and we assume that
    /// the catalyst then catalyzes only forward reaction.
    /// The following is meaning of coefficients:
    ///     kf -  is forward  multiplier for a catalyst C
    ///     kfe - is forward  multiplier for a catalyst E(C) - enantiomer of C
    ///     kb -  is backward multiplier for a catalyst C
    ///     kbe - is backward multiplier for a catalyst E(C)
    ///     etc.
    ///
    /// All 8 combinations are as follows:
    /// kf, kb: T = (R, C, U), E(T) = (E(R), E(C), E(U))
    /// kfe, kbe: T = (R, E(C), U), E(T) = (E(R), C, E(U))
    /// kfu, kbu: T = (R, C, E(U)), E(T) = (E(R), E(C), EU)
    /// kfeu, kbeu: T = (R, E(C), E(U)), E(T) = (E(R), C, U)
    let calculateEnCatRates<'R, 'C, 'S, 'RCS> (i : EnCatRatesInfo<'R, 'C, 'S, 'RCS>) : RelatedReactions<'RCS> =
        let re = (i.reaction, i.getCatEnantiomer i.enCatalyst, i.energySource) |> i.enCatReactionCreator
        let ru = (i.reaction, i.enCatalyst, i.getEnergySourceEnantiomer i.energySource) |> i.enCatReactionCreator
        let reu = (i.reaction, i.getCatEnantiomer i.enCatalyst, i.getEnergySourceEnantiomer i.energySource) |> i.enCatReactionCreator

        let (rf, rb, rfe, rbe), (rfu, rbu, rfeu, rbeu) =
            let k =
                match i.rateGenerationType with
                | RandomChoice -> i.eeParams.rateMultiplierDistr.nextDouble i.rnd

            match k, i.eeParams.enEeDistribution with
            | Some k0, Some df ->
                let s0 = i.getBaseRates i.reaction

                // Forward ee.
                let f = df.nextDouble i.rnd
                let fe = df.nextDouble i.rnd
                let fu = df.nextDouble i.rnd
                let feu = df.nextDouble i.rnd

                let kf = k0 * (1.0 + f)
                let kfe = k0 * (1.0 + fe)
                let kfu = k0 * (1.0 + fu)
                let kfeu = k0 * (1.0 + feu)

                // Backward rates are unaffected. if that changes then plug in the logic here.
                let kb = 1.0
                let kbe = 1.0
                let kbu = 1.0
                let kbeu = 1.0

                let (rf, rfe) =
                    match s0.forwardRate with
                    | Some (ReactionRate sf) -> (kf * sf |> ReactionRate |> Some, kfe * sf |> ReactionRate |> Some)
                    | None -> (None, None)

                let (rb, rbe) =
                    match s0.backwardRate with
                    | Some (ReactionRate sb) -> (kb * sb |> ReactionRate |> Some, kbe * sb |> ReactionRate |> Some)
                    | None -> (None, None)

                let (rfu, rfeu) =
                    match s0.forwardRate with
                    | Some (ReactionRate sf) -> (kfu * sf |> ReactionRate |> Some, kfeu * sf |> ReactionRate |> Some)
                    | None -> (None, None)

                let (rbu, rbeu) =
                    match s0.backwardRate with
                    | Some (ReactionRate sb) -> (kbu * sb |> ReactionRate |> Some, kbeu * sb |> ReactionRate |> Some)
                    | None -> (None, None)

                (rf, rb, rfe, rbe), (rfu, rbu, rfeu, rbeu)
            | _ -> (None, None, None, None), (None, None, None, None)

        {
            primary = { forwardRate = rf; backwardRate = rb }
            similar =
                [
                    { reaction = re; rateData = { forwardRate = rfe; backwardRate = rbe } }
                    { reaction = ru; rateData = { forwardRate = rfu; backwardRate = rbu } }
                    { reaction = reu; rateData = { forwardRate = rfeu; backwardRate = rbeu } }
                ]
        }


    type EnCatRatesSimilarityParam =
        {
            enCatRatesSimGeneration : CatRatesSimGeneration
            getRateMultiplierDistr : RateMultiplierDistributionGetter
            getForwardEeDistr : EeDistributionGetter
            getBackwardEeDistr : EeDistributionGetter
        }


    type AcRateType =
        | AcNone
        | AcForwardRateOnly
        | AcBackwardRateOnly

//        // Not implemented yet.
////        | AcBothRates
////        | AcSameRates


    type AcCatRatesNoneEeParam =
        | AcCatRatesNoneEeParam

        member p.rateMult gt rnd = None
        member p.rateMultiplierDistr = NoneRateMult


    type AcCatRatesFwdEeParam =
        {
            rateMultiplierDistr : RateMultiplierDistribution
            acFwdEeDistribution : EeDistribution option
        }

        static member defaultValue =
            {
                rateMultiplierDistr = NoneRateMult
                acFwdEeDistribution = None
            }


        member p.rateMult gt rnd =
            match gt with
            | RandomChoice ->
                match p.rateMultiplierDistr.nextDouble rnd, p.acFwdEeDistribution with
                | Some k0, Some d ->
                    let ee = d.nextDouble rnd
                    {
                        kf = k0 * (1.0 + ee)
                        kfe = k0 * (1.0 - ee)
                        kb = 1.0
                        kbe = 1.0
                    }
                    |> Some
                | _ -> None


//    type AcCatRatesBkwEeParam =
//        {
//            rateMultiplierDistr : RateMultiplierDistribution
//            acBkwEeDistribution : EeDistribution option
//        }
//
//        static member defaultValue =
//            {
//                rateMultiplierDistr = NoneRateMult
//                acBkwEeDistribution = None
//            }
//
//        member p.rateMult gt rnd =
//            match gt with
//            | RandomChoice ->
//                match p.rateMultiplierDistr.nextDouble rnd, p.acBkwEeDistribution with
//                | Some k0, Some d ->
//                    let ee = d.nextDouble rnd
//                    {
//                        kf = 1.0
//                        kfe = 1.0
//                        kb = k0 * (1.0 + ee)
//                        kbe = k0 * (1.0 - ee)
//                    }
//                    |> Some
//                | _ -> None

// Not implemented yet.
//    type AcCatRatesBothEeParam =
//        {
//            rateMultiplierDistr : RateMultiplierDistribution
//            acFwdEeDistribution : EeDistribution option
//            acBkwEeDistribution : EeDistribution option
//        }
//
//        static member defaultValue =
//            {
//                rateMultiplierDistr = NoneRateMult
//                acFwdEeDistribution = None
//                acBkwEeDistribution = None
//            }
//
//
//    type AcCatRatesSameEeParam =
//        {
//            rateMultiplierDistr : RateMultiplierDistribution
//            acEeDistribution : EeDistribution option
//        }
//
//        static member defaultValue =
//            {
//                rateMultiplierDistr = NoneRateMult
//                acEeDistribution = None
//            }


    /// Specifies how activated catalysts affect base reactions, e,g, A <-> B.
    ///     AcForwardRateOnly:  affects only A + C* -> B + C
    ///     AcBackwardRateOnly: affects only A + C* <- B + C
    ///     AcBothRates:        affects both A + C* <-> B + C but they have different distributions.
    ///     AcSameRates:        affects both A + C* <-> B + C but in the same way (they have the same multiplier).
    type AcCatRatesEeParam =
        | AcNoneParam of AcCatRatesNoneEeParam
        | AcForwardRateOnlyParam of AcCatRatesFwdEeParam

        // Not implemented yet.
//        | AcBackwardRateOnlyParam of AcCatRatesBkwEeParam
//        | AcBothRates of AcCatRatesBothEeParam
//        | AcSameRates of AcCatRatesSameEeParam

//        member a.rateMultiplierDistr =
//            match a with
//            | AcForwardRateOnly p -> p.rateMultiplierDistr
//            | AcBackwardRateOnly p -> p.rateMultiplierDistr
//
//
//        member a.eeDistribution =
//            match a with
//            | AcForwardRateOnly p -> p.acFwdEeDistribution
//            | AcBackwardRateOnly p -> p.acBkwEeDistribution

        member a.rateMult gt rnd =
            let v =
                (gt, rnd)
                ||>
                match a with
                | AcNoneParam p -> p.rateMult
                | AcForwardRateOnlyParam p -> p.rateMult

//            match v with
//            | Some x -> printfn $"AcCatRatesEeParam.rateMult: v = {x}."
//            | None -> ()
            v

        member a.rateMultiplierDistr =
            match a with
            | AcNoneParam p -> p.rateMultiplierDistr
            | AcForwardRateOnlyParam p -> p.rateMultiplierDistr

        static member defaultValue = AcNoneParam AcCatRatesNoneEeParam


    type AcCatRatesSimilarityParam =
        {
            acCatRatesSimGeneration : CatRatesSimGeneration
            getRateMultiplierDistr : RateMultiplierDistributionGetter
            getForwardEeDistr : EeDistributionGetter
            getBackwardEeDistr : EeDistributionGetter
        }

    type AcCatRatesInfoProxy<'R, 'CA, 'C, 'RCA, 'RA> =
        {
            getNonActivated : 'CA -> 'C
            getCatEnantiomer : 'CA -> 'CA
            acCatReactionCreator : ('R * 'CA) -> 'RCA
            getBaseRates : 'R -> RateData // Get rates of base (not catalyzed) reaction.
            createActivationData : 'C -> ReactionRateData<'RA> // Get or creates rates of activation reaction.
            getAcEnantiomer : 'RA -> 'RA // Creates an enantiomer of activation reaction.
            rateGenerationType : RateGenerationType
            rnd : RandomValueGetter
        }


    /// Here we assume that only catalysts, which participate in reaction with activation
    /// can be activated. Otherwise will would end up with all peptides being activated and most of them
    /// not participating in any further reactions.
    ///
    /// 'R - base reaction
    /// 'CA - activated catalyst (C*)
    /// 'C - deactivated catalyst (C)
    /// 'RCA - reaction catalysed by activated catalyst, e.g. A + C* -> B + C
    /// 'RA - activation reaction e.g.: C + Z -> C* + W
    type AcCatRatesInfo<'R, 'CA, 'C, 'RCA, 'RA> =
        {
            reaction : 'R
            acCatalyst : 'CA
            acEeParams : AcCatRatesEeParam
            proxy : AcCatRatesInfoProxy<'R, 'CA, 'C, 'RCA, 'RA>
        }


    /// Thermodynamic constraints are not applicable here because the reaction looks like:
    /// A + C* -> B + C
    /// and so the activated catalyst is not conserved, but rather is transformed into non-activated one.
    let calculateAcCatRates<'R, 'CA, 'C, 'RCA, 'RA> (i : AcCatRatesInfo<'R, 'CA, 'C, 'RCA, 'RA>) : RelatedAcReactions<'RCA, 'RA> =
        let re = (i.reaction, i.proxy.getCatEnantiomer i.acCatalyst) |> i.proxy.acCatReactionCreator
//        printfn $"calculateAcCatRates: Starting, re = {re}"

        let rf, rb, rfe, rbe, ra =
            match  i.acEeParams.rateMult i.proxy.rateGenerationType i.proxy.rnd with
            | Some r ->
                let s0 = i.proxy.getBaseRates i.reaction

                let (rf, rfe) =
                    match s0.forwardRate with
                    | Some (ReactionRate sf) -> (r.kf * sf |> ReactionRate |> Some, r.kfe * sf |> ReactionRate |> Some)
                    | None -> (None, None)

                let (rb, rbe) =
                    match s0.backwardRate with
                    | Some (ReactionRate sb) -> (r.kb * sb |> ReactionRate |> Some, r.kbe * sb |> ReactionRate |> Some)
                    | None -> (None, None)

                let g() =
                    let c = i.proxy.getNonActivated i.acCatalyst
                    let ra = i.proxy.createActivationData c
//                    printfn $"calculateAcCatRates: g: c = {c}, ra = {ra}"
                    [ ra ]

//                printfn $"calculateAcCatRates: rf = {rf}, rb = {rb}, rfe = {rfe}, rbe = {rbe}"

                let a =
                    match rf, rfe, rb, rbe with
                    | Some _, Some _, _, _ -> g()
                    | _, _, Some _, Some _ -> g()
                    | _ -> []

                (rf, rb, rfe, rbe, a)
            | _ -> (None, None, None, None, [])

        {
            acPrimary = { forwardRate = rf; backwardRate = rb }
            acSimilar = [ { reaction = re; rateData = { forwardRate = rfe; backwardRate = rbe } } ]
            activationData = ra
        }
