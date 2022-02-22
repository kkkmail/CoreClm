--declare @clmDefaultValueId bigint
--set @clmDefaultValueId = 4005000110

drop function if exists dbo.getSugarScarcity
go

create function dbo.getSugarScarcity(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select m.[value] as [value]
		--select a.[key] as aKey, b.[key] as bKey, g.[key] as gKey, i.[value] as iValue, j.[value] as jValue, k.[key] as kKey, k.[value] as kValue, l.[key] as lKey, l.[value] as lValue, m.[value] as mValue
		from openjson(@json) a
			cross apply openjson(a.[value]) as b
			cross apply openjson(b.[value]) as c
			cross apply openjson(c.[value]) as d
			cross apply openjson(d.[value]) as e
			cross apply openjson(e.[value]) as f
			cross apply openjson(f.[value]) as g
			cross apply openjson(g.[value]) as h
			cross apply openjson(h.[value]) as i
			cross apply openjson(i.[value]) as j
			cross apply openjson(j.[value]) as k
			cross apply openjson(k.[value]) as l
			cross apply openjson(l.[value]) as m
		where
			a.[key] = 'rateParams'
			and c.[key] = 'Fields'
			and e.[key] = 'Fields'

			-- Can't yet get it better than this:
			--and b.[key] = 4

			and g.[key] = 'sugarSynthesisDistribution'
			and h.[key] = 'Fields'
			and k.[key] = 'threshold'
			and l.[key] = 'Fields'
	)

	--select * from t1
	select @retval = cast(t1.[value] as float)
	from t1

	return @retval
end
go
