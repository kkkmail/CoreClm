use clm604
go

declare @now datetime
set @now = getdate()

;with w as
(
	select
		runQueueOrder
		,d.clmDefaultValueId
		,progress
		,runQueueStatusId
		,errorMessage
		,case when startedOn is not null and progress > 0 then dateadd(second, datediff(second, startedOn, @now) / progress, startedOn) else null end as estCompl
		,cast(case when startedOn is not null and progress > 0 then datediff(second, startedOn, @now) / progress / (3600 * 24) else null end as money) as totalRunTime
		,runQueueId
		,workerNodeName
		,q.modifiedOn
	from
		ClmDefaultValue d 
		inner join ClmTask t on d.clmDefaultValueId = t.clmDefaultValueId
		inner join ModelData m on m.clmTaskId = t.clmTaskId
		inner join RunQueue q on q.modelDataId = m.modelDataId
		left outer join WorkerNode w on q.workerNodeId = w.workerNodeId
	where progress <> 1 and runQueueStatusId = 2
)
select * from w
where
	estCompl is not null
	--and clmDefaultValueId >= 4004000000
	--and estCompl < dateadd(day, 1, @now)
	--and totalRunTime > 0.5
order by estCompl desc
