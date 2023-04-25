--declare @defaultValueId bigint
--set @defaultValueId = 4005000110

drop function if exists clm.getSugarBackward
go

create function clm.getSugarBackward(@defaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select i.[value] as [value]
		from openjson(@json) a
			cross apply openjson(a.[value]) as b
			cross apply openjson(b.[value]) as c
			cross apply openjson(c.[value]) as d
			cross apply openjson(d.[value]) as e
			cross apply openjson(e.[value]) as f
			cross apply openjson(f.[value]) as g
			cross apply openjson(g.[value]) as h
			cross apply openjson(h.[value]) as i
		where 
			a.[key] = 'rateParams'
			and c.[key] = 'Fields'
			and e.[key] = 'Fields'

			-- Can't yet get it better than this:
			and b.[key] = 4

			and g.[key] = 'backwardScale'
			and h.[key] = 'Fields'
	)

	select @retval = cast(t1.[value] as float)
	from t1

	return @retval
end
go
