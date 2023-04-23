drop function if exists dbo.getCatLigMult
go

create function dbo.getCatLigMult(@clmDefaultValueId bigint)
returns float
as
begin
	declare @retVal float
	set @retVal = isnull(dbo.getCatLigMultSim(@clmDefaultValueId), dbo.getCatLigMultRnd(@clmDefaultValueId))
	return @retval
end
go

