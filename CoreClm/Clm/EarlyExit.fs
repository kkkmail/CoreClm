namespace Clm

open System

//open Primitives.GeneralPrimitives
//open Primitives.SolverPrimitives
open Softellect.Sys
open Softellect.Sys.Core
open Softellect.Sys.Primitives
open Softellect.Sys.AppSettings
open Softellect.Wcf.Common
open Softellect.Wcf.Service
open Softellect.Messaging.Primitives
open Softellect.Messaging.ServiceInfo
open Softellect.Messaging.Service
open Softellect.Messaging.Client
open Softellect.Messaging.Errors

//open ClmSys.MessagingData
open ClmSys.SolverRunnerPrimitives
open Primitives.VersionInfo
//open ClmSys.WorkerNodeData
//open ContGenServiceInfo.ServiceInfo
open Clm.CalculationData
open Clm.ModelParams
open ClmSys.ContGenPrimitives
//open ClmSys.WorkerNodePrimitives
//open ClmSys.PartitionerPrimitives
open ClmSys.ClmErrors
//open ClmSys.GeneralPrimitives
open Clm.ChartData
//open Primitives.GeneralData
open ClmSys.SolverData

module EarlyExit =

    type EarlyExitData = ChartData


    type EarlyExitRule =
        | ProgressExceeds of decimal
        | MaxWeightedAverageAbsEeExceeds of float
        | MaxLastEeExceeds of float
        | MaxAverageEeExceeds of float
        | RunTimeExceeds of TimeSpan

        member r.isValid (d : EarlyExitData) =
            match r with
            | ProgressExceeds p -> d.progress > p, $"progress: %A{d.progress} > %A{p}"
            | MaxWeightedAverageAbsEeExceeds e ->
                d.eeData.maxWeightedAverageAbsEe > e, $"maxWeightedAverageAbsEe: %A{d.eeData.maxWeightedAverageAbsEe} > %A{e}"
            | MaxLastEeExceeds e -> d.eeData.maxLastEe > e, $"maxLastEe: %A{d.eeData.maxLastEe} > %A{e}"
            | MaxAverageEeExceeds e -> d.eeData.maxAverageEe > e, $"maxAverageEe: %A{d.eeData.maxAverageEe} > %A{e}"
            | RunTimeExceeds e -> (DateTime.Now - d.startedOn) > e, $"runTime exceeds: {e}"
            ||> bindBool


    /// Any rule can be satisfied.
    type AnyRule =
        | AnyRule of list<EarlyExitRule>

        member e.value = let (AnyRule v) = e in v


    /// Outer list - all collections must be satisfied, inner list (from AnyRule) - at least one rule must be satisfied.
    type EarlyExitRuleCollection =
        | AllOfAny of list<AnyRule>


    type EarlyExitStrategy =
        | AnyRuleCollection of list<EarlyExitRuleCollection> // Any of the collections can be satisfied.

        member e.exitEarly d =
            match e with
            | AnyRuleCollection c ->
                match c with
                | [] -> false, None // If there are no collections, then early exit strategy cannot work.
                | _ ->
                    let r v =
                        match v with
                        | [] -> false, None // If outer list is empty, then early exit strategy cannot work.
                        | _ ->
                            let g m1 m2 =
                                match m1, m2 with
                                | Some m1, Some m2 -> m1 + ", " + m2 |> Some
                                | Some m1, None -> Some m1
                                | None, Some m2 -> Some m2
                                | None, None -> None

                            let combineOr (r1, m1) (r2, m2) = r1 || r2, g m1 m2

                            let foldInner (a : AnyRule) =
                                a.value |> List.fold (fun acc b -> combineOr (b.isValid d) acc) (false, None)

                            let combineAnd (r1, m1) (r2, m2) = r1 && r2, g m1 m2

                            let result =
                                v
                                |> List.map foldInner
                                |> List.fold combineAnd (true, None)

                            result

                    let chooser (AllOfAny v) =
                        let x = r v
                        match x with
                        | false, _ -> None
                        | true, _ -> Some x

                    c |> List.tryPick chooser |> Option.defaultValue (false, None)

        static member getEeValue p e =
            [
                [
                    ProgressExceeds p
                ]
                |> AnyRule

                [
                    MaxWeightedAverageAbsEeExceeds e
                    MaxLastEeExceeds e
                    MaxAverageEeExceeds e
                ]
                |> AnyRule
            ]
            |> AllOfAny

        static member quickValue p =
                EarlyExitStrategy.getEeValue p.quickProgress p.quickMinEe

        static member standardValue p =
                EarlyExitStrategy.getEeValue p.standardProgress p.standardMinEe

        static member slowValue p =
                EarlyExitStrategy.getEeValue p.slowProgress p.slowMinEe

        static member longRunningValue p =
            [
                [ RunTimeExceeds p.maxRunTime ] |> AnyRule
            ]
            |> AllOfAny

        static member getValue p =
            [
                EarlyExitStrategy.quickValue
                EarlyExitStrategy.standardValue
                EarlyExitStrategy.slowValue
                EarlyExitStrategy.longRunningValue
            ]
            |> List.map (fun e -> e p)
            |> AnyRuleCollection


    type EarlyExitInfo =
        {
            frequency : EarlyExitCheckFrequency
            earlyExitStrategy : EarlyExitStrategy
        }

        static member getValue p =
            {
                frequency = p.earlyExitCheckFreq
                earlyExitStrategy = EarlyExitStrategy.getValue p
            }
