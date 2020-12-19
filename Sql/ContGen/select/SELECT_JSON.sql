declare @clmDefaultValueId bigint
set @clmDefaultValueId = 9000

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
		where  c.[key] = 'Fields'
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
		where b.[key] = 'catDestrParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catDestrRndEeParams'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
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

	--,t11 as
	--(
	--	select a.* 
	--	from t10
	--	cross apply openjson(t10.[value]) as a
	--)
	--select * from t11
	--select @retval = cast(a.[value] as float)
	--from t8
	--cross apply openjson(t8.[value]) as a


select @retVal

--select json_value(@json, '$.rateParams.ArrayValue[0]')
--select json_query(@json, '$.rateParams.ArrayValue[0]')

--select json_value(@json, '$.rateParams[0]')

--select 1 as id
--cross apply openjson(json_query(@json, '$.rateParams')) as x
--cross apply openjson(x.[value], '$') as y

--select json_value(@json, '$.rateParams[0].Case')
--select json_query(@json, '$.rateParams[0].Case')

--select * 
--from dbo.JSONHierarchy(@json, DEFAULT, DEFAULT, DEFAULT)
--where ValueType <> 'array'

