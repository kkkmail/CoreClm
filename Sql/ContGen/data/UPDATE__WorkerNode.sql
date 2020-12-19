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

