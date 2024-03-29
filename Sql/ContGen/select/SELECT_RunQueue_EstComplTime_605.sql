use clm605
go

declare @now datetime
set @now = getdate()

;with w as
(
	select
		runQueueOrder
		,progress / callCount as averageStep
		,d.clmDefaultValueId
		,progress
		,yRelative
		,runQueueStatusId
		,errorMessage
		,case 
			when runQueueStatusId = 3 then q.modifiedOn
			when startedOn is not null and progress > 0 then dateadd(second, datediff(second, startedOn, @now) / progress, startedOn) 
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
	estCompl is not null
	--and clmDefaultValueId >= 4004000000
	--and estCompl < dateadd(day, 1, @now)
	--and totalRunTime > 0.5
order by estCompl desc
