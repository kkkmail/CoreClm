namespace ClmImpure.ReactionRateModels

open System.Collections.Generic
open Clm.ReactionRatesBase

open ClmImpure.ReactionRateFunctions

module ReactionRateModelBase =

    [<AbstractClass>]
    type RateModel<'P, 'R when 'R : equality> (p : 'P) =
        let rateDictionaryImpl = Dictionary<'R, RateData>()
        member _.rateDictionary = rateDictionaryImpl
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl rateDictionaryImpl
