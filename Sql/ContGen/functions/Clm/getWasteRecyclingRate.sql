drop function if exists dbo.getWasteRecyclingRate
go

create function dbo.getWasteRecyclingRate(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	select @retVal = cast(b.[value] as float)
	from t1
	cross apply openjson(t1.[value]) as a
	cross apply openjson(a.[value]) as b
	where b.[key] = 'wasteRecyclingRate'

	return @retVal
end
go
