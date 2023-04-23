drop function if exists dbo.getCatLigScarcity
go

create function dbo.getCatLigScarcity(@clmDefaultValueId bigint)
returns float
as
begin
	declare @retVal float
	set @retVal = isnull(dbo.getCatLigScarcitySim(@clmDefaultValueId), dbo.getCatLigScarcityRnd(@clmDefaultValueId))
	return @retval
end
go

