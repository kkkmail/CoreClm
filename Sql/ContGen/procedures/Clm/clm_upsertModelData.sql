drop procedure if exists dbo.clm_upsertModelData
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure dbo.clm_upsertModelData 
		@modelDataId uniqueidentifier, 
		@taskId uniqueidentifier, 
		@seedValue int, 
		@modelDataParams nvarchar(max), 
		@modelBinaryData varbinary(max), 
		@createdOn datetime
as
begin
	declare @rowCount int
	set nocount on;

    merge clm.ModelData as target
    using (select @modelDataId, @taskId, @seedValue, @modelDataParams, @modelBinaryData, @createdOn)
    as source (modelDataId, taskId, seedValue, modelDataParams, modelBinaryData, createdOn)
    on (target.modelDataId = source.modelDataId)
    when not matched then
        insert (modelDataId, taskId, seedValue, modelDataParams, modelBinaryData, createdOn)
        values (source.modelDataId, source.taskId, source.seedValue, source.modelDataParams, source.modelBinaryData, source.createdOn)
    when matched then
        update set taskId = source.taskId, seedValue = source.seedValue, modelDataParams = source.modelDataParams, modelBinaryData = source.modelBinaryData, createdOn = source.createdOn;

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

