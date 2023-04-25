drop procedure if exists eeInf.upsertModelData
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure eeInf.upsertModelData 
		@modelDataId uniqueidentifier, 
		@modelDataParams nvarchar(max), 
		@modelBinaryData varbinary(max), 
		@createdOn datetime
as
begin
	declare @rowCount int
	set nocount on;

    merge eeInf.ModelData as target
    using (select @modelDataId, @modelDataParams, @modelBinaryData, @createdOn)
    as source (modelDataId, modelDataParams, modelBinaryData, createdOn)
    on (target.modelDataId = source.modelDataId)
    when not matched then
        insert (modelDataId, modelDataParams, modelBinaryData, createdOn)
        values (source.modelDataId, source.modelDataParams, source.modelBinaryData, source.createdOn)
    when matched then
        update set modelDataParams = source.modelDataParams, modelBinaryData = source.modelBinaryData, createdOn = source.createdOn;

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

