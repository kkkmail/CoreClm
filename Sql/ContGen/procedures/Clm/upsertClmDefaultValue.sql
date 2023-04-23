drop procedure if exists upsertClmDefaultValue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure upsertClmDefaultValue 
		@clmDefaultValueId bigint,
		@defaultRateParams nvarchar(max),
		@description nvarchar(max)
as
begin
	declare @rowCount int
	set nocount on;

    merge ClmDefaultValue as target
    using (select @clmDefaultValueId, @defaultRateParams, @description) as source (clmDefaultValueId, defaultRateParams, description)
    on (target.clmDefaultValueId = source.clmDefaultValueId)
    when not matched then
        insert (clmDefaultValueId, defaultRateParams, description)
        values (source.clmDefaultValueId, source.defaultRateParams, source.description)
    when matched then
        update set defaultRateParams = source.defaultRateParams, description = source.description;

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

