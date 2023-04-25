drop function if exists dbo.getAvailableWorkerNode
go


create function dbo.getAvailableWorkerNode(@lastAllowedNodeErrInMinutes int)
returns table
as
return
(
	with a as
	(
	select
		workerNodeId
		,nodePriority
		,cast(
			case
				when numberOfCores <= 0 then 1
				else (select count(1) as runningModels from RunQueue where workerNodeId = w.workerNodeId and runQueueStatusId in (2, 5, 7)) / (cast(numberOfCores as money))
			end as money) as workLoad
		,case when lastErrorOn is null or dateadd(minute, @lastAllowedNodeErrInMinutes, lastErrorOn) < getdate() then 0 else 1 end as noErr
	from WorkerNode w
	where isInactive = 0
	),
	b as
	(
		select
			a.*, 
			c.new_id
			from a
			cross apply (select new_id from vw_newid) c
	)
	select top 1
	workerNodeId
	from b
	where noErr = 0 and workLoad < 1
	order by nodePriority desc, workLoad, new_id
)
go

