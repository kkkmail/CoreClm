use wns605
go

declare @now datetime
set @now = getdate()

;with w as
(
	select
		runQueueOrder
		,progress
		,runQueueStatusId
		,errorMessage
		,case when startedOn is not null and progress > 0 then dateadd(second, datediff(second, startedOn, modifiedOn) / progress, startedOn) else null end as estCompl
		,cast(case when startedOn is not null and progress > 0 then datediff(second, startedOn, modifiedOn) / progress / (3600 * 24) else null end as money) as totalRunTime
		,runQueueId
		,modifiedOn
	from
		RunQueue q
	where progress <> 1 and runQueueStatusId = 2
)
select * from w
where
	estCompl is not null
order by estCompl desc
