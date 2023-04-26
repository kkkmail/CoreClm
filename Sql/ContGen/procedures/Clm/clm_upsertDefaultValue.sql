drop procedure if exists dbo.clm_upsertDefaultValue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure dbo.clm_upsertDefaultValue 
		@defaultValueId bigint,
		@defaultRateParams nvarchar(max),
		@description nvarchar(max)
as
begin
	declare @rowCount int
	set nocount on;

    merge clm.DefaultValue as target
    using (select @defaultValueId, @defaultRateParams, @description) as source (defaultValueId, defaultRateParams, description)
    on (target.defaultValueId = source.defaultValueId)
    when not matched then
        insert (defaultValueId, defaultRateParams, description)
        values (source.defaultValueId, source.defaultRateParams, source.description)
    when matched then
        update set defaultRateParams = source.defaultRateParams, description = source.description;

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

