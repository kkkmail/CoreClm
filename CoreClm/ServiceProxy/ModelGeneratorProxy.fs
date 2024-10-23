namespace ServiceProxy

open Clm.ModelParams
open Clm.CalculationData
open DbData.DatabaseTypesDbo
open DbData.DatabaseTypesClm
open ClmSys.ClmErrors
open Clm.Generator.ClmModelData

module ModelGeneratorProxy =

    type GenerateModelProxy =
        {
            loadParams : ClmTask -> ClmResult<AllParams>
            upsertModelData : ModelData -> UnitResult
            //upsertRunQueue : RunQueue -> UnitResult
            updateClmTask : ClmTask -> UnitResult
        }

        static member create u coll so c =
            {
                loadParams = AllParams.create u coll so (loadClmDefaultValue c)
                upsertModelData = upsertModelData c
                //upsertRunQueue = upsertRunQueue c
                updateClmTask = updateClmTask c
            }


    type GenerateAllProxy =
        {
            loadIncompleteClmTasks : unit -> ListResult<ClmTask>
            generateModel : ClmTask -> UnitResult
        }
