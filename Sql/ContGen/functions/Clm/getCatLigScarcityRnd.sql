drop function if exists clm.getCatLigScarcityRnd
go

create function clm.getCatLigScarcityRnd(@defaultValueId bigint)
returns float
as
begin
	--declare @defaultValueId bigint
	--set @defaultValueId = 4000000000

	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

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
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catLigRndEeParams'
	)
	--,t4 as
	--(
	--	select a.* 
	--	from t3
	--	cross apply openjson(t3.[value]) as a
	--	where a.[key] = 'catDestrRndEeParams'
	--)
	,t5 as
	(
		select a.*
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E06
	from t10
	cross apply openjson(t10.[value]) as a

	--print ('@retval = ' + isnull(cast(@retval as nvarchar(20)), '<null>'))

	return @retval
end
go

