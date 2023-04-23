drop procedure if exists upsertClmModelData
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure upsertClmModelData 
		@clmModelDataId uniqueidentifier, 
		@clmTaskId uniqueidentifier, 
		@seedValue int, 
		@modelDataParams nvarchar(max), 
		@modelBinaryData varbinary(max), 
		@createdOn datetime
as
begin
	declare @rowCount int
	set nocount on;

    merge ClmModelData as target
    using (select @clmModelDataId, @clmTaskId, @seedValue, @modelDataParams, @modelBinaryData, @createdOn)
    as source (clmModelDataId, clmTaskId, seedValue, modelDataParams, modelBinaryData, createdOn)
    on (target.clmModelDataId = source.clmModelDataId)
    when not matched then
        insert (clmModelDataId, clmTaskId, seedValue, modelDataParams, modelBinaryData, createdOn)
        values (source.clmModelDataId, source.clmTaskId, source.seedValue, source.modelDataParams, source.modelBinaryData, source.createdOn)
    when matched then
        update set clmTaskId = source.clmTaskId, seedValue = source.seedValue, modelDataParams = source.modelDataParams, modelBinaryData = source.modelBinaryData, createdOn = source.createdOn;

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

