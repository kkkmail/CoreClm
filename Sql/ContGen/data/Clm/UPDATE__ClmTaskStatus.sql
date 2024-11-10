;with 
	valTbl as
	(
		select * 
		from 
		( values
			  (0, 'Active')
			, (1, 'Inactive')

		) as a (taskStatusId, taskStatusName)
	)
insert into clm.TaskStatus
select valTbl.*
from valTbl
left outer join clm.TaskStatus on valTbl.taskStatusId = clm.TaskStatus.TaskStatusId
where clm.TaskStatus.TaskStatusId is null
go


