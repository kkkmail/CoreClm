use clm607
go


select runQueueId
      ,runQueueOrder
	  ,clmDefaultValueId
      --,modelDataId
      ,runQueueStatusId
      ,y0
      ,tEnd
      --,useAbundant
      ,errorMessage
      ,progress
      ,callCount
      ,yRelative
      --,maxEe
      --,maxAverageEe
      --,maxWeightedAverageAbsEe
      ,maxLastEe
      ,workerNodeName
      ,q.createdOn
      ,startedOn
      ,q.modifiedOn
FROM RunQueue q
inner join dbo.ModelData m on q.modelDataId = m.modelDataId
inner join dbo.ClmTask t on m.clmTaskId = t.clmTaskId
inner join dbo.WorkerNode w on q.workerNodeId = w.workerNodeId
where runQueueStatusId <> 3
	--and t.clmDefaultValueId = 4005000082
order by maxLastEe desc

-- update RunQueue set runQueueStatusId = 0, errorMessage = null, workerNodeId = null, startedOn = null, modifiedOn = getdate() where runQueueStatusId = 4 and runQueueId in ('8E41105D-EEB8-42A8-BB2A-B6F318236B88', 'D8CE8318-12F2-4CC7-9920-9CF22BA34D33')
-- update RunQueue set runQueueStatusId = 0, errorMessage = null, workerNodeId = null, startedOn = null, modifiedOn = getdate() where runQueueStatusId = 4 and runQueueId in ('4EDA8CDE-F4C9-4785-BA5A-832BDF36A777')


