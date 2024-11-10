;with 
	valTbl as
	(
		select * 
		from 
		( values
			  (1, 'ClmModel')
			, (2, 'EeInfModel')

		) as a (modelTypeId, modelTypeName)
	)
insert into ModelType
select valTbl.*
from valTbl
left outer join ModelType on valTbl.modelTypeId = ModelType.modelTypeId
where ModelType.modelTypeId is null
go

