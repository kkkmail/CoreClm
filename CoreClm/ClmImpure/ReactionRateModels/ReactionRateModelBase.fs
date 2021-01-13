namespace ClmImpure.ReactionRateModels

open System.Collections.Generic
open Clm.ReactionRatesBase

open ClmImpure.ReactionRateFunctions

module ReactionRateModelBase =

    [<AbstractClass>]
    type RateModel<'P, 'R, 'C when 'R : equality> (p : 'P, getKeyOpt : ('R -> 'C) option) =
        let dictionaryDataImpl : DictionaryData<'R, 'C> =
            let d = Dictionary<'R, RateData>()

            match getKeyOpt with
            | None ->
                {
                    keySetData = None
                    rateDictionary = d
                }
            | Some g ->
                {
                    keySetData =
                        {
                            keySet = new HashSet<'C>()
                            getReactionKey = g
                        }
                        |> Some
                    rateDictionary = d
                }

        new (p) = RateModel (p, None)
        member _.dictionaryData = dictionaryDataImpl
        member _.rateDictionary = dictionaryDataImpl.rateDictionary
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl dictionaryDataImpl.rateDictionary


    type RateModel<'P, 'R when 'R : equality> = RateModel<'P, 'R, 'R>
