drop function if exists clm.getCatLigMult
go

create function clm.getCatLigMult(@defaultValueId bigint)
returns float
as
begin
	declare @retVal float
	set @retVal = isnull(clm.getCatLigMultSim(@defaultValueId), clm.getCatLigMultRnd(@defaultValueId))
	return @retval
end
go

