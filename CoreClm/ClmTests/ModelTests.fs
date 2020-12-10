namespace ClmTests

open System
open Xunit
open Xunit.Abstractions
open Should
open DbData.DatabaseTypes
open DbData.Configuration
open Clm.Model.ModelData


type ModelTests(output : ITestOutputHelper) =

    let writeLine s = output.WriteLine s


    [<Fact>]
    member _.ModelDataShouldMatchGeneratedCode () : unit =
        let eps = 0.000001
        let cgModelDataParamsWithExtraData = modelDataParamsWithExtraData
        let cgGetTotalSubst = getTotalSubst
        let cgGetTotals = getTotals
        let cgUpdate = update

        let modelDataId = cgModelDataParamsWithExtraData.regularParams.modelDataParams.modelInfo.modelDataId
        let mdo = loadModelData getClmConnectionString modelDataId

        let rnd = Random()
        let x = [| for _ in 1..cgModelDataParamsWithExtraData.regularParams.allSubstData.allSubst.Length -> rnd.NextDouble() |]

        let md =
            match mdo with
            | Ok md ->
                let paramEq = (md.modelData.modelDataParams = cgModelDataParamsWithExtraData.regularParams.modelDataParams)
                writeLine (sprintf "(md.modelData.modelDataParams = cgModelDataParamsWithExtraData.modelDataParams) =\n    %A\n" paramEq)

                let mdModelDataParamsWithExtraData = md.modelData.getModelDataParamsWithExtraData()
                let allParamEq = (mdModelDataParamsWithExtraData.regularParams = cgModelDataParamsWithExtraData.regularParams)
                writeLine (sprintf "(mdAllParam.regularParams = cgModelDataParamsWithExtraData.regularParams) =\n    %A\n" allParamEq)

                let mdGetTotalSubst = md.modelData.modelBinaryData.calculationData.getTotalSubst
                let mdGetTotals = md.modelData.modelBinaryData.calculationData.getTotals
                let mdUpdate = md.modelData.modelBinaryData.calculationData.getDerivative

                let cgTotalSubst = cgGetTotalSubst x
                let mdTotalSubst = mdGetTotalSubst x
                writeLine (sprintf "diff (must be close to 0.0) = %A" (cgTotalSubst - mdTotalSubst))

                let cgGetTotals = cgGetTotals x
                let mdGetTotals = mdGetTotals x
                let diffTotals =
                    Array.zip cgGetTotals mdGetTotals
                    |> Array.map(fun ((a1, b1), (a2, b2)) -> (a1 - a2) * (a1 - a2) + (b1 - b2) * (b1 - b2))
                    |> Array.sum

                writeLine (sprintf "diffTotals (must be close to 0.0) = %A" diffTotals)

                let cgUpdate = cgUpdate x |> Array.toList
                let mdUpdate = mdUpdate x |> Array.toList

                let diffUpdate =
                    List.zip cgUpdate mdUpdate
                    |> List.map(fun (a, b) -> (a - b) * (a - b))
                    |> List.sum

                writeLine ( sprintf "diffUpdate (must be close to 0.0) = %A" diffUpdate)

                if diffUpdate > eps
                then
                    List.zip cgUpdate mdUpdate
                    |> List.mapi(fun i (a, b) -> i, (a, b, abs (a - b)))
                    |> List.filter (fun (i, (a, b, c)) -> c > eps)
                    |> List.sortByDescending (fun (i, (a, b, c)) -> c, i)
                    |> List.map (fun (i, (a, b, c)) -> writeLine (sprintf "i = %A, s = %A, cg = %A, md = %A, diff = %A" i (allSubst.[i]) a b c))
                    |> ignore

                diffUpdate.ShouldBeLessThan eps
                md
            | Error e ->
                writeLine (sprintf "Failed to load model data with error: %A." e)
                failwith "! Error occurred !"

        let mdUpdate = md.modelData.modelBinaryData.calculationData.getDerivative
        let repetitions = [for i in 1..1_000 -> i ]

//        printfn "\n\nCG"
        repetitions |> List.map (fun _ -> cgUpdate x |> ignore) |> ignore
//        printfn "CG completed\n\n"

//        printfn "MD"
        repetitions |> List.map (fun _ -> mdUpdate x |> ignore) |> ignore
//        printfn "MD completed\n\n"
//        printfn "... completed."

        ()
