namespace Clm

open FSharp.Collections
open Clm.Distributions
open Clm.ReactionTypes

module ReactionRatesBase =

    /// Specifies how to generate rates.
    /// RandomChoice first randomly determine the reactions with non-zero rates and then gets these rates (without using threshold).
    type RateGenerationType =
        | RandomChoice


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

        override r.ToString() = sprintf "{ f: %A; b: %A }" r.forwardRate r.backwardRate


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
            a : array<'A>
            b : array<'B>
            reactionName : ReactionName
            successNumberType : SuccessNumberType
        }


    type RateGeneratorInfo<'A, 'B, 'C> =
        {
            a : array<'A>
            b : array<'B>
            c : array<'C>
            reactionName : ReactionName
            successNumberType : SuccessNumberType
        }


    type RelatedReactions<'R> =
        {
            primary : RateData
            similar : list<ReactionRateData<'R>>
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
