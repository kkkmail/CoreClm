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
	and clmDefaultValueId = 4005000005
	--and estCompl < dateadd(day, 1, @now)
	--and totalRunTime > 0.5
	--and runQueueId in ('50CC3AC9-1C1C-4C93-BCF4-E4B79E788B09', 'BEDA7AB8-AC50-4258-9193-988CF31A5D99', '93B0683D-D63D-4612-A220-0CAC718AC420', '176D2FEC-511B-4A34-A98F-7AE52D12BB51')
order by estCompl desc
