drop procedure if exists upsertEeInfModelData
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure upsertEeInfModelData 
		@eeInfModelDataId uniqueidentifier, 
		@modelDataParams nvarchar(max), 
		@modelBinaryData varbinary(max), 
		@createdOn datetime
as
begin
	declare @rowCount int
	set nocount on;

    merge EeInfModelData as target
    using (select @eeInfModelDataId, @modelDataParams, @modelBinaryData, @createdOn)
    as source (eeInfModelDataId, modelDataParams, modelBinaryData, createdOn)
    on (target.eeInfModelDataId = source.eeInfModelDataId)
    when not matched then
        insert (eeInfModelDataId, modelDataParams, modelBinaryData, createdOn)
        values (source.eeInfModelDataId, source.modelDataParams, source.modelBinaryData, source.createdOn)
    when matched then
        update set modelDataParams = source.modelDataParams, modelBinaryData = source.modelBinaryData, createdOn = source.createdOn;

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

