;with 
	valTbl as
	(
		select * 
		from 
		( values
			  (0, 'Active')
			, (1, 'Inactive')

		) as a (clmTaskStatusId, clmTaskStatusName)
	)
insert into ClmTaskStatus
select valTbl.*
from valTbl
left outer join ClmTaskStatus on valTbl.clmTaskStatusId = ClmTaskStatus.clmTaskStatusId
where ClmTaskStatus.clmTaskStatusId is null
go


;with 
	valTbl as
	(
		select * 
		from 
		( values
			  (0, 'NotStarted')
			, (1, 'Inactive')
			, (7, 'RunRequested')
			, (2, 'InProgress')
			, (3, 'Completed')
			, (4, 'Failed')
			, (5, 'CancelRequested')
			, (6, 'Cancelled')

		) as a (runQueueStatusId, runQueueStatusName)
	)
insert into RunQueueStatus
select valTbl.*
from valTbl
left outer join RunQueueStatus on valTbl.runQueueStatusId = RunQueueStatus.runQueueStatusId
where RunQueueStatus.runQueueStatusId is null
go


;with 
	valTbl as
	(
		select * 
		from 
		( values
			  (cast('3262C12A-2E1B-4D6D-B787-655FDD779BF9' as uniqueidentifier), 'LOCAL', 1)

		) as a (workerNodeId, workerNodeName, isLocal)
	)
insert into WorkerNode (workerNodeId, workerNodeName, isLocal)
select valTbl.*
from valTbl
left outer join WorkerNode on valTbl.workerNodeId = WorkerNode.workerNodeId
where WorkerNode.workerNodeId is null
go

;with 
	valTbl as
	(
		select * 
		from 
		( values
			  (0, 'GuaranteedDelivery')
			, (1, 'NonGuaranteedDelivery')

		) as a (deliveryTypeId, deliveryTypeName)
	)
insert into DeliveryType
select valTbl.*
from valTbl
left outer join DeliveryType on valTbl.deliveryTypeId = DeliveryType.deliveryTypeId
where DeliveryType.deliveryTypeId is null
go


