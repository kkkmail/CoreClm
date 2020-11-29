namespace ServiceProxy

open Clm.ModelParams
open Clm.CalculationData
open DbData.DatabaseTypes
open ClmSys.ClmErrors
open Clm.Generator.ClmModelData

module ModelGeneratorProxy =

    type GenerateModelProxy =
        {
            loadParams : ClmTask -> ClmResult<AllParams>
            upsertModelData : ModelData -> UnitResult
            upsertRunQueue : RunQueue -> UnitResult
            updateClmTask : ClmTask -> UnitResult
        }

        static member create c =
            {
                loadParams = AllParams.create (loadClmDefaultValue c)
                upsertModelData = upsertModelData c
                upsertRunQueue = upsertRunQueue c
                updateClmTask = updateClmTask c
            }


    type GenerateAllProxy =
        {
            loadIncompleteClmTasks : unit -> ListResult<ClmTask>
            generateModel : ClmTask -> UnitResult
        }
