drop function if exists dbo.getLigBkw
go

create function dbo.getLigBkw(@clmDefaultValueId bigint)
returns float
as
begin
	--declare @clmDefaultValueId bigint
	--set @clmDefaultValueId = 4002000022

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
		select a.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'ligationDistribution'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'backwardScale'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)

	select @retval = cast(a.[value] as float)
	from t5
	cross apply openjson(t5.[value]) as a

	--print ('@retval = ' + isnull(cast(@retval as nvarchar(20)), '<null>'))
	--select * from t5

	return @retval
end
go

