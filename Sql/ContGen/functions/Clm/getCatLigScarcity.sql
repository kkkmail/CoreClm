drop function if exists clm.getCatLigScarcity
go

create function clm.getCatLigScarcity(@defaultValueId bigint)
returns float
as
begin
	declare @retVal float
	set @retVal = isnull(clm.getCatLigScarcitySim(@defaultValueId), clm.getCatLigScarcityRnd(@defaultValueId))
	return @retval
end
go

