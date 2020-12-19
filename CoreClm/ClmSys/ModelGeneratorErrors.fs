namespace ClmSys

open ClmSys.ContGenPrimitives

module ModelGeneratorErrors =

    type GenerateModelCodeError =
        | UnableLoadParamsErr of ClmTaskId
        | UnableSaveModelCodeErr of ClmTaskId


    type GenerateModelError =
        | UnableLoadParamsErr of ClmTaskId
        | UnableUpsertModelDataErr of ClmTaskId
        | TaskCompletedErr of ClmTaskId


    type ModelGeneratorError =
        | GenerateModelCodeErr of GenerateModelCodeError
        | GenerateModelErr of GenerateModelError
