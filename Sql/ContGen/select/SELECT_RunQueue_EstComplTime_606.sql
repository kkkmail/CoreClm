use clm606
go

declare @now datetime
set @now = getdate()

;with w as
(
	select
		runQueueOrder
		,d.clmDefaultValueId
		,case when callCount > 0 then progress / callCount else 0 end as averageStep
		,progress
		,yRelative
		,callCount
		,runQueueStatusId
		,errorMessage
		,case 
			when runQueueStatusId = 3 then q.modifiedOn
			when startedOn is not null and progress > 0 and datediff(second, startedOn, @now) / progress < 2147483647 then dateadd(second, datediff(second, startedOn, @now) / progress, startedOn) 
			else null 
		 end as estCompl
		,cast(
			case
				when runQueueStatusId = 3 then datediff(second, q.startedOn, q.modifiedOn) / (3600.0 * 24.0)
				when startedOn is not null and progress > 0 then datediff(second, startedOn, @now) / progress / (3600.0 * 24.0) 
				else null 
			end as money) as totalRunTime
		,runQueueId
		,workerNodeName
		,q.startedOn
		,q.modifiedOn
	from
		ClmDefaultValue d 
		inner join ClmTask t on d.clmDefaultValueId = t.clmDefaultValueId
		inner join ModelData m on m.clmTaskId = t.clmTaskId
		inner join RunQueue q on q.modelDataId = m.modelDataId
		left outer join WorkerNode w on q.workerNodeId = w.workerNodeId
	--where progress <> 1 and runQueueStatusId = 2
)
select * 
from w
where
	1 = 1
	--and estCompl is null
	and estCompl is not null
	and runQueueStatusId not in (3)
	--and clmDefaultValueId >= 4004000000
	--and estCompl < dateadd(day, 1, @now)
	--and totalRunTime > 0.5
order by estCompl desc
