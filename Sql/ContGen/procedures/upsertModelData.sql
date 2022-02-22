drop procedure if exists upsertModelData
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure upsertModelData 
		@modelDataId uniqueidentifier, 
		@clmTaskId uniqueidentifier, 
		@seedValue int, 
		@modelDataParams nvarchar(max), 
		@modelBinaryData varbinary(max), 
		@createdOn datetime
as
begin
	declare @rowCount int
	set nocount on;

    merge ModelData as target
    using (select @modelDataId, @clmTaskId, @seedValue, @modelDataParams, @modelBinaryData, @createdOn)
    as source (modelDataId, clmTaskId, seedValue, modelDataParams, modelBinaryData, createdOn)
    on (target.modelDataId = source.modelDataId)
    when not matched then
        insert (modelDataId, clmTaskId, seedValue, modelDataParams, modelBinaryData, createdOn)
        values (source.modelDataId, source.clmTaskId, source.seedValue, source.modelDataParams, source.modelBinaryData, source.createdOn)
    when matched then
        update set clmTaskId = source.clmTaskId, seedValue = source.seedValue, modelDataParams = source.modelDataParams, modelBinaryData = source.modelBinaryData, createdOn = source.createdOn;

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

