﻿namespace ClmImpure.ReactionRateModels

open Clm.ReactionRateParams
open ClmImpure.ReactionRateModels.LigationRandomModel

module LigationModel =

    type LigationModel =
        | LigRndModel of LigationRandomModel

        member model.getRates rnd r =
            match model with
            | LigRndModel m ->
//                printfn $"LigationModel.getRates: r = {r}."
                m.getRates rnd r

        member model.tryGetRates r =
            match model with
            | LigRndModel m -> m.tryGetRates r

        member model.inputParams =
            match model with
            | LigRndModel m -> m.inputParams |> LigRndParam

        member model.getAllRates() =
            match model with
            | LigRndModel m -> m.getAllRates()

        static member create p =
            match p with
            | LigRndParam q -> LigationRandomModel q |> LigRndModel
