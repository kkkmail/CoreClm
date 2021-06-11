use [clm607]
go


select [runQueueId]
      ,[runQueueOrder]
      --,[modelDataId]
      ,[runQueueStatusId]
      ,[y0]
      ,[tEnd]
      --,[useAbundant]
      ,[errorMessage]
      ,[progress]
      ,[callCount]
      ,[yRelative]
      --,[maxEe]
      --,[maxAverageEe]
      --,[maxWeightedAverageAbsEe]
      ,[maxLastEe]
      ,[workerNodeName]
      ,q.[createdOn]
      ,[startedOn]
      ,q.[modifiedOn]
FROM RunQueue q
inner join dbo.ModelData m on q.modelDataId = m.modelDataId
inner join dbo.ClmTask t on m.clmTaskId = t.clmTaskId
inner join dbo.WorkerNode w on q.workerNodeId = w.workerNodeId
where runQueueStatusId <> 3 
	--and t.clmDefaultValueId = 4005000026
order by [maxLastEe] desc

-- update RunQueue set runQueueStatusId = 0, errorMessage = null, workerNodeId = null, startedOn = null, modifiedOn = getdate() where runQueueId = 'C60AD91C-A9CC-47A3-A69C-C796C0A6D063'


