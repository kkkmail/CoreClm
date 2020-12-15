--declare @clmDefaultValueId bigint
--set @clmDefaultValueId = 9028

drop function if exists dbo.getCatSynthSim
go

create function dbo.getCatSynthSim(@clmDefaultValueId bigint)
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
		where b.[key] = 'catSynthSimParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catRatesSimGeneration'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t6 as
	(
		select d.*
		from t5
		cross apply openjson(t5.[value]) as a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		cross apply openjson(c.[value]) as d
		where b.[key] = 'Fields' and d.[key] = 'distributionParams'
	)
	,t7 as
	(
		select a.*
		from t6
		cross apply openjson(t6.[value]) as a
		where a.[key] = 'threshold'
	)
	,t8 as
	(
		select a.* 
		from t7
		cross apply openjson(t7.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float)
	from t8
	cross apply openjson(t8.[value]) as a

	return @retval
end
go
