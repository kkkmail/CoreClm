declare @clmDefaultValueId bigint, @defaultRateParams nvarchar(max), @description nvarchar(2000), @fileStructureVersion money

merge ClmDefaultValue as target
using (select @clmDefaultValueId, @defaultRateParams, @description, @fileStructureVersion) as source (clmDefaultValueId, defaultRateParams, description, fileStructureVersion)  
on (target.clmDefaultValueId = source.clmDefaultValueId)
when not matched then
    insert (clmDefaultValueId, defaultRateParams, description, fileStructureVersion)
    values (source.clmDefaultValueId, source.defaultRateParams, source.description, source.fileStructureVersion)
when matched then
    update set defaultRateParams = source.defaultRateParams, description = source.description, fileStructureVersion = source.fileStructureVersion;
go

----------------------------------

declare @modelDataId uniqueidentifier, @clmTaskId uniqueidentifier, @fileStructureVersion money, 
	@seedValue int, @modelDataParams nvarchar(max), @modelBinaryData varbinary(max), @createdOn datetime

merge ModelData as target
using (select @modelDataId, @clmTaskId, @fileStructureVersion, @seedValue, @modelDataParams, @modelBinaryData, @createdOn) 
as source (modelDataId, clmTaskId, fileStructureVersion, seedValue, modelDataParams, modelBinaryData, createdOn)  
on (target.modelDataId = source.modelDataId)
when not matched then
    insert (modelDataId, clmTaskId, fileStructureVersion, seedValue, modelDataParams, modelBinaryData, createdOn)
    values (source.modelDataId, source.clmTaskId, source.fileStructureVersion, source.seedValue, source.modelDataParams, source.modelBinaryData, source.createdOn)
when matched then
    update set clmTaskId = source.clmTaskId, fileStructureVersion = source.fileStructureVersion, seedValue = source.seedValue, modelDataParams = source.modelDataParams, modelBinaryData = source.modelBinaryData, createdOn = source.createdOn;
go

----------------------------------

declare @modelDataId uniqueidentifier, @clmTaskId uniqueidentifier, @parentModelDataId uniqueidentifier, @createdOn datetime

merge ModelData as target
using (select @modelDataId, @clmTaskId, @parentModelDataId, @createdOn) 
as source (modelDataId, clmTaskId, parentModelDataId, createdOn)  
on (target.modelDataId = source.modelDataId)
when not matched then
    insert (modelDataId, clmTaskId, parentModelDataId, createdOn)
    values (source.modelDataId, source.clmTaskId, source.parentModelDataId, source.createdOn)
when matched then
    update set clmTaskId = source.clmTaskId, parentModelDataId = source.parentModelDataId, createdOn = source.createdOn;
go
