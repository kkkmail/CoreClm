declare @node nvarchar(50)
set @node = 'NODE'

update RunQueue
set
	workerNodeId = null,
	runQueueStatusId = 0
where runQueueId in
	(	
		select
			q.runQueueId
		from
			RunQueue q
			left outer join WorkerNode w on q.workerNodeId = w.workerNodeId
		where w.workerNodeName = @node and q.runQueueStatusId = 2
	)
