if not exists(select schema_name from information_schema.schemata where schema_name = 'clm') begin
	print 'Creating schema clm...'
	exec sp_executesql N'create schema clm'
end else begin
	print 'Schema clm already exists...'
end
go

if not exists(select schema_name from information_schema.schemata where schema_name = 'eeInf') begin
	print 'Creating schema eeInf...'
	exec sp_executesql N'create schema eeInf'
end else begin
	print 'Schema eeInf already exists...'
end
go

IF OBJECT_ID('[dbo].[ModelType]') IS NULL begin
	print 'Creating table [dbo].[ModelType] ...'

	CREATE TABLE [dbo].[ModelType](
		[modelTypeId] [int] NOT NULL,
		[modelTypeName] [nvarchar](50) NOT NULL,
	 CONSTRAINT [PK_ModelType] PRIMARY KEY CLUSTERED 
	(
		[modelTypeId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_ModelType] ON [dbo].[ModelType]
	(
		[modelTypeName] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [dbo].[ModelType] already exists ...'
end
go


IF OBJECT_ID('[dbo].[RunQueueStatus]') IS NULL begin
	print 'Creating table [dbo].[RunQueueStatus] ...'

	CREATE TABLE [dbo].[RunQueueStatus](
		[runQueueStatusId] [int] NOT NULL,
		[runQueueStatusName] [nvarchar](50) NOT NULL,
	 CONSTRAINT [PK_RunQueueStatus] PRIMARY KEY CLUSTERED 
	(
		[runQueueStatusId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_RunQueueStatus] ON [dbo].[RunQueueStatus]
	(
		[runQueueStatusName] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [dbo].[RunQueueStatus] already exists ...'
end
go


IF OBJECT_ID('[dbo].[WorkerNode]') IS NULL begin
	print 'Creating table [dbo].[WorkerNode] ...'

	CREATE TABLE [dbo].[WorkerNode](
		[workerNodeId] [uniqueidentifier] NOT NULL,
		[workerNodeOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[workerNodeName] [nvarchar](100) NOT NULL,
		[nodePriority] [int] NOT NULL,
		[numberOfCores] [int] NOT NULL,
		[description] [nvarchar](1000) NULL,
		[isLocal] [bit] NOT NULL,
		[isInactive] [bit] NOT NULL,
		[createdOn] [datetime] NOT NULL,
		[modifiedOn] [datetime] NOT NULL,
		[lastErrorOn] [datetime] NULL,
	 CONSTRAINT [PK_WorkerNode] PRIMARY KEY CLUSTERED 
	(
		[workerNodeId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [dbo].[WorkerNode] ADD  CONSTRAINT [DF_WorkerNode_isLocal]  DEFAULT ((0)) FOR [isLocal]
	ALTER TABLE [dbo].[WorkerNode] ADD  CONSTRAINT [DF__WorkerNod__isInactive]  DEFAULT ((0)) FOR [isInactive]
	ALTER TABLE [dbo].[WorkerNode] ADD  CONSTRAINT [DF__WorkerNod__createdOn]  DEFAULT (getdate()) FOR [createdOn]
	ALTER TABLE [dbo].[WorkerNode] ADD  CONSTRAINT [DF__WorkerNod__modifiedOn]  DEFAULT (getdate()) FOR [modifiedOn]
	ALTER TABLE [dbo].[WorkerNode] ADD  CONSTRAINT [DF__WorkerNod__nodePriority]  DEFAULT ((100)) FOR [nodePriority]
	ALTER TABLE [dbo].[WorkerNode] ADD  CONSTRAINT [DF__WorkerNod__numberOfCores]  DEFAULT ((0)) FOR [numberOfCores]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_WorkerNodeName] ON [dbo].[WorkerNode]
	(
		[workerNodeName] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_WorkerNodeOrder] ON [dbo].[WorkerNode]
	(
		[workerNodeOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [dbo].[WorkerNode] already exists ...'
end
go

IF OBJECT_ID('[dbo].[RunQueue]') IS NULL begin
	print 'Creating table [dbo].[RunQueue] ...'

	CREATE TABLE [dbo].[RunQueue](
		[runQueueId] [uniqueidentifier] NOT NULL,
		[runQueueOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[modelTypeId] [int] NOT NULL,
		[workerNodeId] [uniqueidentifier] NULL,
		[runQueueStatusId] [int] NOT NULL,
		[errorMessage] nvarchar(max) NULL,
		[progress] [decimal](18, 14) NOT NULL,
		[callCount] [bigint] NOT NULL,
		[relativeInvariant] [float] NOT NULL, -- Should be close to 1.0 all the time. Substantial deviations is a sign of errors. If not needed, then set to 1.0.
		[createdOn] [datetime] NOT NULL,
		[modifiedOn] [datetime] NOT NULL,
		[startedOn] [datetime] NULL,
	 CONSTRAINT [PK_RunQueue] PRIMARY KEY CLUSTERED 
	(
		[runQueueId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [dbo].[RunQueue] ADD  DEFAULT ((0)) FOR [runQueueStatusId]
	ALTER TABLE [dbo].[RunQueue] ADD  DEFAULT ((0)) FOR [progress]
	ALTER TABLE [dbo].[RunQueue] ADD  DEFAULT ((1)) FOR [relativeInvariant]
	ALTER TABLE [dbo].[RunQueue] ADD  DEFAULT (getdate()) FOR [createdOn]
	ALTER TABLE [dbo].[RunQueue] ADD  DEFAULT (getdate()) FOR [modifiedOn]

	ALTER TABLE [dbo].[RunQueue]  WITH CHECK ADD  CONSTRAINT [FK_RunQueue_RunQueueStatus] FOREIGN KEY([runQueueStatusId])
	REFERENCES [dbo].[RunQueueStatus] ([runQueueStatusId])
	ALTER TABLE [dbo].[RunQueue] CHECK CONSTRAINT [FK_RunQueue_RunQueueStatus]

	ALTER TABLE [dbo].[RunQueue]  WITH CHECK ADD  CONSTRAINT [FK_RunQueue_ModelType] FOREIGN KEY([modelTypeId])
	REFERENCES [dbo].[ModelType] ([modelTypeId])
	ALTER TABLE [dbo].[RunQueue] CHECK CONSTRAINT [FK_RunQueue_ModelType]

	ALTER TABLE [dbo].[RunQueue]  WITH CHECK ADD  CONSTRAINT [FK_RunQueue_WorkerNode] FOREIGN KEY([workerNodeId])
	REFERENCES [dbo].[WorkerNode] ([workerNodeId])
	ALTER TABLE [dbo].[RunQueue] CHECK CONSTRAINT [FK_RunQueue_WorkerNode]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_RunQueue] ON [dbo].[RunQueue]
	(
		[runQueueOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

end else begin
	print 'Table [dbo].[RunQueue] already exists ...'
end
go



IF OBJECT_ID('[clm].[TaskStatus]') IS NULL begin
	print 'Creating table [clm].[TaskStatus] ...'

	CREATE TABLE [clm].[TaskStatus](
		[taskStatusId] [int] NOT NULL,
		[taskStatusName] [nvarchar](50) NOT NULL,
	 CONSTRAINT [PK_clm_TaskStatus] PRIMARY KEY CLUSTERED 
	(
		[taskStatusId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_clm_TaskStatus] ON [clm].[TaskStatus]
	(
		[taskStatusName] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [clm].[TaskStatus] already exists ...'
end
go


IF OBJECT_ID('[clm].[Task]') IS NULL begin
	print 'Creating table [clm].[Task] ...'

	CREATE TABLE [clm].[Task](
		[taskId] [uniqueidentifier] NOT NULL,
		[taskOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[defaultValueId] [bigint] NOT NULL,
		[taskStatusId] [int] NOT NULL,
		[taskPriority] [int] NOT NULL,
		[numberOfAminoAcids] [int] NOT NULL,
		[maxPeptideLength] [int] NOT NULL,
		[numberOfRepetitions] [int] NOT NULL,
		[remainingRepetitions] [int] NOT NULL,
		[createdOn] [datetime] NOT NULL,
		[modifiedOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_ClmTask] PRIMARY KEY CLUSTERED 
	(
		[taskId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [clm].[Task] ADD  DEFAULT ((1)) FOR [numberOfRepetitions]
	ALTER TABLE [clm].[Task] ADD  DEFAULT ((1)) FOR [remainingRepetitions]
	ALTER TABLE [clm].[Task] ADD  DEFAULT ((0)) FOR [taskStatusId]
	ALTER TABLE [clm].[Task] ADD  DEFAULT ((1000)) FOR [taskPriority]
	ALTER TABLE [clm].[Task] ADD  DEFAULT (getdate()) FOR [createdOn]
	ALTER TABLE [clm].[Task] ADD  DEFAULT (getdate()) FOR [modifiedOn]

	ALTER TABLE [clm].[Task]  WITH CHECK ADD CONSTRAINT [FK_clm_Task_TaskStatus] FOREIGN KEY([taskStatusId])
	REFERENCES [clm].[TaskStatus] ([taskStatusId])
	ALTER TABLE [clm].[Task] CHECK CONSTRAINT [FK_clm_Task_TaskStatus]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_clm_Task] ON [clm].[Task]
	(
		[taskOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [clm].[Task] already exists ...'
end
go



IF OBJECT_ID('[clm].[ModelData]') IS NULL begin
	print 'Creating table [clm].[ModelData] ...'

	CREATE TABLE [clm].[ModelData](
		[modelDataId] [uniqueidentifier] NOT NULL,
		[modelDataOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[taskId] [uniqueidentifier] NOT NULL,
		[seedValue] [int] NULL,
		[modelDataParams] [nvarchar](max) NOT NULL,
		[modelBinaryData] [varbinary](max) NOT NULL,
		[createdOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_clm_ModelData] PRIMARY KEY CLUSTERED 
	(
		[modelDataId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

	ALTER TABLE [clm].[ModelData] ADD  DEFAULT (getdate()) FOR [createdOn]

	ALTER TABLE [clm].[ModelData]  WITH CHECK ADD  CONSTRAINT [FK_clm_ModelData_Task] FOREIGN KEY([taskId])
	REFERENCES [clm].[Task] ([taskId])
	ALTER TABLE [clm].[ModelData] CHECK CONSTRAINT [FK_clm_ModelData_Task]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_clm_ModelData] ON [clm].[ModelData]
	(
		[modelDataOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [clm].[ModelData] already exists ...'
end
go




IF OBJECT_ID('[clm].[RunQueue]') IS NULL begin
	print 'Creating table [clm].[RunQueue] ...'

	CREATE TABLE [clm].[RunQueue](
		[runQueueId] [uniqueidentifier] NOT NULL,
		[modelDataId] [uniqueidentifier] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL,
		[maxEe] [float] NOT NULL,
		[maxAverageEe] [float] NOT NULL,
		[maxWeightedAverageAbsEe] [float] NOT NULL,
		[maxLastEe] [float] NOT NULL,
	 CONSTRAINT [PK_clm_RunQueue] PRIMARY KEY CLUSTERED 
	(
		[runQueueId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [clm].[RunQueue] ADD  DEFAULT ((0)) FOR [useAbundant]
	ALTER TABLE [clm].[RunQueue] ADD  DEFAULT ((0)) FOR [maxEe]
	ALTER TABLE [clm].[RunQueue] ADD  DEFAULT ((0)) FOR [maxAverageEe]
	ALTER TABLE [clm].[RunQueue] ADD  DEFAULT ((0)) FOR [maxWeightedAverageAbsEe]
	ALTER TABLE [clm].[RunQueue] ADD  DEFAULT ((0)) FOR [maxLastEe]

	ALTER TABLE [clm].[RunQueue]  WITH CHECK ADD  CONSTRAINT [FK_clm_RunQueue_RunQueue] FOREIGN KEY([runQueueId])
	REFERENCES [dbo].[RunQueue] ([runQueueId])
	ALTER TABLE [clm].[RunQueue] CHECK CONSTRAINT [FK_clm_RunQueue_RunQueue]

	ALTER TABLE [clm].[RunQueue]  WITH CHECK ADD  CONSTRAINT [FK_clm_RunQueue_ModelData] FOREIGN KEY([modelDataId])
	REFERENCES [clm].[ModelData] ([modelDataId])
	ALTER TABLE [clm].[RunQueue] CHECK CONSTRAINT [FK_clm_RunQueue_ModelData]

end else begin
	print 'Table [clm].[ClmRunQueue] already exists ...'
end
go



IF OBJECT_ID('[clm].[CommandLineParam]') IS NULL begin
	print 'Creating table [clm].[CommandLineParam] ...'

	CREATE TABLE [clm].[CommandLineParam](
		[commandLineParamId] [uniqueidentifier] NOT NULL,
		[commandLineParamOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[taskId] [uniqueidentifier] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL,
		[createdOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_clm_CommandLineParam] PRIMARY KEY CLUSTERED 
	(
		[commandLineParamId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [clm].[CommandLineParam] ADD DEFAULT ((0)) FOR [useAbundant]
	ALTER TABLE [clm].[CommandLineParam] ADD DEFAULT (getdate()) FOR [createdOn]

	ALTER TABLE [clm].[CommandLineParam]  WITH CHECK ADD  CONSTRAINT [FK_clm_CommandLineParam_Task] FOREIGN KEY([taskId])
	REFERENCES [clm].[Task] ([taskId])

	ALTER TABLE [clm].[CommandLineParam] CHECK CONSTRAINT [FK_clm_CommandLineParam_Task]

	CREATE NONCLUSTERED INDEX [UX_clm_CommandLineParam] ON [clm].[CommandLineParam]
	(
		[commandLineParamOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [clm].[CommandLineParam] already exists ...'
end
go



IF OBJECT_ID('[clm].[DefaultValue]') IS NULL begin
	print 'Creating table [clm].[DefaultValue] ...'

	CREATE TABLE [clm].[DefaultValue](
		[defaultValueId] [bigint] NOT NULL,
		[defaultRateParams] [nvarchar](max) NOT NULL,
		[description] [nvarchar](max) NULL,
	 CONSTRAINT [PK_clm_DefaultValue] PRIMARY KEY CLUSTERED 
	(
		[defaultValueId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
end else begin
	print 'Table [clm].[DefaultValue] already exists ...'
end
go


IF OBJECT_ID('[clm].[ResultData]') IS NULL begin
	print 'Creating table [clm].[ResultData] ...'

	CREATE TABLE [clm].[ResultData](
		[resultDataId] [uniqueidentifier] NOT NULL, -- resultId is always runQueueId
		[resultDataOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[modelDataId] [uniqueidentifier] NOT NULL,
		--[workerNodeId] [uniqueidentifier] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL,
		[maxEe] [float] NOT NULL,
		[maxAverageEe] [float] NOT NULL,
		[maxWeightedAverageAbsEe] [float] NOT NULL,
		[maxLastEe] [float] NOT NULL,
		[createdOn] [datetime] NOT NULL,
		[modifiedOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_clm_ResultData] PRIMARY KEY CLUSTERED 
	(
		[resultDataId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [clm].[ResultData] ADD  DEFAULT ((0)) FOR [useAbundant]
	ALTER TABLE [clm].[ResultData] ADD  DEFAULT ((0)) FOR [maxEe]
	ALTER TABLE [clm].[ResultData] ADD  DEFAULT ((0)) FOR [maxAverageEe]
	ALTER TABLE [clm].[ResultData] ADD  DEFAULT ((0)) FOR [maxWeightedAverageAbsEe]
	ALTER TABLE [clm].[ResultData] ADD  DEFAULT ((0)) FOR [maxLastEe]
	ALTER TABLE [clm].[ResultData] ADD  DEFAULT (getdate()) FOR [createdOn]
	ALTER TABLE [clm].[ResultData] ADD  DEFAULT (getdate()) FOR [modifiedOn]

	--ALTER TABLE [clm].[ResultData]  WITH CHECK ADD  CONSTRAINT [FK_clm_ResultData_WorkerNode] FOREIGN KEY([workerNodeId])
	--REFERENCES [clm].[WorkerNode] ([workerNodeId])
	--ALTER TABLE [clm].[ResultData] CHECK CONSTRAINT [FK_clm_ResultData_WorkerNode]

	ALTER TABLE [clm].[ResultData]  WITH CHECK ADD  CONSTRAINT [FK_clm_ResultlData_ModelData] FOREIGN KEY([modelDataId])
	REFERENCES [clm].[ModelData] ([modelDataId])
	ALTER TABLE [clm].[ResultData] CHECK CONSTRAINT [FK_clm_ResultlData_ModelData]

	ALTER TABLE [clm].[ResultData]  WITH CHECK ADD  CONSTRAINT [FK_clm_ResultData_RunQueue] FOREIGN KEY([resultDataId])
	REFERENCES [clm].[RunQueue] ([runQueueId])
	ALTER TABLE [clm].[resultData] CHECK CONSTRAINT [FK_clm_ResultData_RunQueue]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_clm_ResultData] ON [clm].[ResultData]
	(
		[resultDataOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

end else begin
	print 'Table [clm].[ResultData] already exists ...'
end
go




IF OBJECT_ID('[eeInf].[ModelData]') IS NULL begin
	print 'Creating table [eeInf].[ModelData] ...'

	CREATE TABLE [eeInf].[ModelData](
		[modelDataId] [uniqueidentifier] NOT NULL,
		[modelDataOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[modelDataParams] [nvarchar](max) NOT NULL,
		[modelBinaryData] [varbinary](max) NOT NULL,
		[createdOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_eeInf_ModelData] PRIMARY KEY CLUSTERED 
	(
		[modelDataId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

	ALTER TABLE [eeInf].[ModelData] ADD  DEFAULT (getdate()) FOR [createdOn]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_eeInf_ModelData] ON [eeInf].[ModelData]
	(
		[modelDataOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [eeInf].[ModelData] already exists ...'
end
go




IF OBJECT_ID('[eeInf].[RunQueue]') IS NULL begin
	print 'Creating table [eeInf].[RunQueue] ...'

	CREATE TABLE [eeInf].[RunQueue](
		[runQueueId] [uniqueidentifier] NOT NULL,
		[ModelDataId] [uniqueidentifier] NOT NULL,
		[tEnd] [money] NOT NULL,
	 CONSTRAINT [PK_eeInf_RunQueue] PRIMARY KEY CLUSTERED 
	(
		[runQueueId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [eeInf].[RunQueue]  WITH CHECK ADD  CONSTRAINT [FK_eeInf_RunQueue_RunQueue] FOREIGN KEY([runQueueId])
	REFERENCES [dbo].[RunQueue] ([runQueueId])
	ALTER TABLE [eeInf].[RunQueue] CHECK CONSTRAINT [FK_eeInf_RunQueue_RunQueue]

	ALTER TABLE [eeInf].[RunQueue]  WITH CHECK ADD  CONSTRAINT [FK_eeInf_RunQueue_ModelData] FOREIGN KEY([modelDataId])
	REFERENCES [eeInf].[ModelData] ([modelDataId])
	ALTER TABLE [eeInf].[RunQueue] CHECK CONSTRAINT [FK_eeInf_RunQueue_ModelData]

end else begin
	print 'Table [eeInf].[RunQueue] already exists ...'
end
go



IF OBJECT_ID('[eeInf].[ResultData]') IS NULL begin
	print 'Creating table [eeInf].[ResultData] ...'

	CREATE TABLE [eeInf].[ResultData](
		[resultDataId] [uniqueidentifier] NOT NULL, -- resultId is always runQueueId
		[resultDataOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[modelDataId] [uniqueidentifier] NOT NULL,
		[createdOn] [datetime] NOT NULL,
		[modifiedOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_eeInf_ResultData] PRIMARY KEY CLUSTERED 
	(
		[resultDataId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [eeInf].[ResultData] ADD  DEFAULT (getdate()) FOR [createdOn]
	ALTER TABLE [eeInf].[ResultData] ADD  DEFAULT (getdate()) FOR [modifiedOn]

	ALTER TABLE [eeInf].[ResultData]  WITH CHECK ADD  CONSTRAINT [FK_eeInf_ResultlData_ModelData] FOREIGN KEY([modelDataId])
	REFERENCES [eeInf].[ModelData] ([modelDataId])
	ALTER TABLE [eeInf].[ResultData] CHECK CONSTRAINT [FK_eeInf_ResultlData_ModelData]

	ALTER TABLE [eeInf].[ResultData]  WITH CHECK ADD  CONSTRAINT [FK_eeInf_ResultData_RunQueue] FOREIGN KEY([resultDataId])
	REFERENCES [eeInf].[RunQueue] ([runQueueId])
	ALTER TABLE [eeInf].[ResultData] CHECK CONSTRAINT [FK_eeInf_ResultData_RunQueue]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_eeInf_ResultData] ON [eeInf].[ResultData]
	(
		[resultDataOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

end else begin
	print 'Table [eeInf].[ResultData] already exists ...'
end
go




drop view if exists vw_newid
go


create view vw_newid
as
select newid() as new_id
go

drop function if exists dbo.getAvailableWorkerNode
go


create function dbo.getAvailableWorkerNode(@lastAllowedNodeErrInMinutes int)
returns table
as
return
(
	with a as
	(
	select
		workerNodeId
		,nodePriority
		,cast(
			case
				when numberOfCores <= 0 then 1
				else (select count(1) as runningModels from RunQueue where workerNodeId = w.workerNodeId and runQueueStatusId in (2, 5, 7)) / (cast(numberOfCores as money))
			end as money) as workLoad
		,case when lastErrorOn is null or dateadd(minute, @lastAllowedNodeErrInMinutes, lastErrorOn) < getdate() then 0 else 1 end as noErr
	from WorkerNode w
	where isInactive = 0
	),
	b as
	(
		select
			a.*, 
			c.new_id
			from a
			cross apply (select new_id from vw_newid) c
	)
	select top 1
	workerNodeId
	from b
	where noErr = 0 and workLoad < 1
	order by nodePriority desc, workLoad, new_id
)
go

drop view if exists vw_AvailableWorkerNode
go


create view vw_AvailableWorkerNode
as
with a as
(
select
	workerNodeId
	,nodePriority
	,isnull(cast(
		case
			when numberOfCores <= 0 then 1
			else (select count(1) as runningModels from RunQueue where workerNodeId = w.workerNodeId and runQueueStatusId in (2, 5, 7)) / (cast(numberOfCores as money))
		end as money), 0) as workLoad
	,case when lastErrorOn is null then null else datediff(minute, getdate(), lastErrorOn) end as lastErrMinAgo
from WorkerNode w
where isInactive = 0
)
select
	a.*, 
	c.new_id as OrderId
	from a
	cross apply (select new_id from vw_newid) c

go

--declare @defaultValueId bigint
--set @defaultValueId = 4005000020

drop function if exists clm.getAcBkwCatLigScarcity
go

create function clm.getAcBkwCatLigScarcity(@defaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'acBkwCatLigParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'acBkwCatLigRndEeParams'
	)
	,t4_1 as
	(
		select a.* 
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t5 as
	(
		select b.*
		from t4_1
		cross apply openjson(t4_1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E09
	from t10
	cross apply openjson(t10.[value]) as a

	--select @retVal

	return @retval
end
go
--declare @defaultValueId bigint
--set @defaultValueId = 4005000020

drop function if exists clm.getAcBkwCatLigSimilarity
go

create function clm.getAcBkwCatLigSimilarity(@defaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'acBkwCatLigSimParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'acCatRatesSimGeneration'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
	)
	,t7 as
	(
		select a.*
		from t6
		cross apply openjson(t6.[value]) as a
		where a.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E09
	from t10
	cross apply openjson(t10.[value]) as a

	--select @retVal

	return @retval
end
go
--declare @defaultValueId bigint
--set @defaultValueId = 4005000020

drop function if exists clm.getAcCatDestrScarcity
go

create function clm.getAcCatDestrScarcity(@defaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'acCatDestrParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'acCatDestrRndEeParams'
	)
	,t4_1 as
	(
		select a.* 
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t5 as
	(
		select b.*
		from t4_1
		cross apply openjson(t4_1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E06
	from t10
	cross apply openjson(t10.[value]) as a

	return @retval
end
go
--declare @defaultValueId bigint
--set @defaultValueId = 4005000020

drop function if exists clm.getAcCatDestrSimilarity
go

create function clm.getAcCatDestrSimilarity(@defaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'acCatDestrSimParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'acCatRatesSimGeneration'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
	)
	,t7 as
	(
		select a.*
		from t6
		cross apply openjson(t6.[value]) as a
		where a.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float)
	from t10
	cross apply openjson(t10.[value]) as a

	--select @retVal

	return @retval
end
go
--declare @defaultValueId bigint
--set @defaultValueId = 4005000020


drop function if exists clm.getAcCatSynthScarcity
go

create function clm.getAcCatSynthScarcity(@defaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'acCatSynthParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'acCatSynthRndEeParams'
	)
	,t4_1 as
	(
		select a.* 
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t5 as
	(
		select b.*
		from t4_1
		cross apply openjson(t4_1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E06
	from t10
	cross apply openjson(t10.[value]) as a

	--select @retVal

	return @retval
end
go
--declare @defaultValueId bigint
--set @defaultValueId = 4005000020

drop function if exists clm.getAcCatSynthSimilarity
go

create function clm.getAcCatSynthSimilarity(@defaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'acCatSynthSimParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'acCatRatesSimGeneration'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
	)
	,t7 as
	(
		select a.*
		from t6
		cross apply openjson(t6.[value]) as a
		where a.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float)
	from t10
	cross apply openjson(t10.[value]) as a

	--select @retVal

	return @retval
end
go
--declare @defaultValueId bigint
--set @defaultValueId = 4005000020


drop function if exists clm.getAcFwdCatLigScarcity
go

create function clm.getAcFwdCatLigScarcity(@defaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'acFwdCatLigParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'acFwdCatLigRndEeParams'
	)
	,t4_1 as
	(
		select a.* 
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t5 as
	(
		select b.*
		from t4_1
		cross apply openjson(t4_1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E09
	from t10
	cross apply openjson(t10.[value]) as a

	--select @retVal

	return @retval
end
go
--declare @defaultValueId bigint
--set @defaultValueId = 4005000020

drop function if exists clm.getAcFwdCatLigSimilarity
go

create function clm.getAcFwdCatLigSimilarity(@defaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'acFwdCatLigSimParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'acCatRatesSimGeneration'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
	)
	,t7 as
	(
		select a.*
		from t6
		cross apply openjson(t6.[value]) as a
		where a.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E09
	from t10
	cross apply openjson(t10.[value]) as a

	--select @retVal

	return @retval
end
go
drop function if exists clm.getCatDestrScarcity
go

create function clm.getCatDestrScarcity(@defaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catDestrParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catDestrRndEeParams'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E06
	from t10
	cross apply openjson(t10.[value]) as a

	return @retval
end
go
--declare @defaultValueId bigint
--set @defaultValueId = 9028

drop function if exists clm.getCatDestrSim
go

create function clm.getCatDestrSim(@defaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catDestrSimParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catRatesSimGeneration'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t6 as
	(
		select d.*
		from t5
		cross apply openjson(t5.[value]) as a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		cross apply openjson(c.[value]) as d
		where b.[key] = 'Fields' and d.[key] = 'distributionParams'
	)
	,t7 as
	(
		select a.*
		from t6
		cross apply openjson(t6.[value]) as a
		where a.[key] = 'threshold'
	)
	,t8 as
	(
		select a.* 
		from t7
		cross apply openjson(t7.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float)
	from t8
	cross apply openjson(t8.[value]) as a

	return @retval
end
go
drop function if exists clm.getCatLigMult
go

create function clm.getCatLigMult(@defaultValueId bigint)
returns float
as
begin
	declare @retVal float
	set @retVal = isnull(clm.getCatLigMultSim(@defaultValueId), clm.getCatLigMultRnd(@defaultValueId))
	return @retval
end
go

drop function if exists clm.getCatLigMultRnd
go

create function clm.getCatLigMultRnd(@defaultValueId bigint)
returns float
as
begin
	--declare @defaultValueId bigint
	--set @defaultValueId = 4000000000

	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catLigRndEeParams'
	)
	--,t4 as
	--(
	--	select a.* 
	--	from t3
	--	cross apply openjson(t3.[value]) as a
	--	where a.[key] = 'catDestrRndEeParams'
	--)
	,t5 as
	(
		select a.*
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'scale'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float)
	from t10
	cross apply openjson(t10.[value]) as a

	--print ('@retval = ' + isnull(cast(@retval as nvarchar(20)), '<null>'))
	--select * from t10

	return @retval
end
go

drop function if exists clm.getCatLigMultSim
go

create function clm.getCatLigMultSim(@defaultValueId bigint)
returns float
as
begin
	--declare @defaultValueId bigint
	--set @defaultValueId = 4002000022

	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catLigParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catLigRndEeParams'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'scale'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float)
	from t10
	cross apply openjson(t10.[value]) as a

	--print ('@retval = ' + isnull(cast(@retval as nvarchar(20)), '<null>'))
	--select * from t10

	return @retval
end
go

drop function if exists clm.getCatLigScarcity
go

create function clm.getCatLigScarcity(@defaultValueId bigint)
returns float
as
begin
	declare @retVal float
	set @retVal = isnull(clm.getCatLigScarcitySim(@defaultValueId), clm.getCatLigScarcityRnd(@defaultValueId))
	return @retval
end
go

drop function if exists clm.getCatLigScarcityRnd
go

create function clm.getCatLigScarcityRnd(@defaultValueId bigint)
returns float
as
begin
	--declare @defaultValueId bigint
	--set @defaultValueId = 4000000000

	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catLigRndEeParams'
	)
	--,t4 as
	--(
	--	select a.* 
	--	from t3
	--	cross apply openjson(t3.[value]) as a
	--	where a.[key] = 'catDestrRndEeParams'
	--)
	,t5 as
	(
		select a.*
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E06
	from t10
	cross apply openjson(t10.[value]) as a

	--print ('@retval = ' + isnull(cast(@retval as nvarchar(20)), '<null>'))

	return @retval
end
go

drop function if exists clm.getCatLigScarcitySim
go

create function clm.getCatLigScarcitySim(@defaultValueId bigint)
returns float
as
begin
	--declare @defaultValueId bigint
	--set @defaultValueId = 4002000022

	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catLigParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catLigRndEeParams'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E06
	from t10
	cross apply openjson(t10.[value]) as a

	--print ('@retval = ' + isnull(cast(@retval as nvarchar(20)), '<null>'))
	--select * from t10

	return @retval
end
go

drop function if exists clm.getCatLigSim
go

create function clm.getCatLigSim(@defaultValueId bigint)
returns float
as
begin
	--declare @defaultValueId bigint
	--set @defaultValueId = 4002000022

	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catLigSimParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catRatesSimGeneration'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t6 as
	(
		select d.*
		from t5
		cross apply openjson(t5.[value]) as a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		cross apply openjson(c.[value]) as d
		where b.[key] = 'Fields' and d.[key] = 'distributionParams'
	)
	,t7 as
	(
		select a.*
		from t6
		cross apply openjson(t6.[value]) as a
		where a.[key] = 'threshold'
	)
	,t8 as
	(
		select a.* 
		from t7
		cross apply openjson(t7.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float)
	from t8
	cross apply openjson(t8.[value]) as a

	--select * from t7

	return @retval
end
go
drop function if exists clm.getCatSynthScarcity
go

create function clm.getCatSynthScarcity(@defaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catSynthParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catSynthRndEeParams'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E06
	from t10
	cross apply openjson(t10.[value]) as a

	return @retval
end
go
--declare @defaultValueId bigint
--set @defaultValueId = 9028

drop function if exists clm.getCatSynthSim
go

create function clm.getCatSynthSim(@defaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catSynthSimParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catRatesSimGeneration'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t6 as
	(
		select d.*
		from t5
		cross apply openjson(t5.[value]) as a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		cross apply openjson(c.[value]) as d
		where b.[key] = 'Fields' and d.[key] = 'distributionParams'
	)
	,t7 as
	(
		select a.*
		from t6
		cross apply openjson(t6.[value]) as a
		where a.[key] = 'threshold'
	)
	,t8 as
	(
		select a.* 
		from t7
		cross apply openjson(t7.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float)
	from t8
	cross apply openjson(t8.[value]) as a

	return @retval
end
go
drop function if exists clm.getGroupId
go

create function clm.getGroupId(@defaultValueId bigint)
returns bigint
as
begin
	return (@defaultValueId / 1000000000)
end
go
drop function if exists clm.getLigBkw
go

create function clm.getLigBkw(@defaultValueId bigint)
returns float
as
begin
	--declare @defaultValueId bigint
	--set @defaultValueId = 4002000022

	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.*
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select a.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'ligationDistribution'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'backwardScale'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)

	select @retval = cast(a.[value] as float)
	from t5
	cross apply openjson(t5.[value]) as a

	--print ('@retval = ' + isnull(cast(@retval as nvarchar(20)), '<null>'))
	--select * from t5

	return @retval
end
go

drop function if exists clm.getLigFwd
go

create function clm.getLigFwd(@defaultValueId bigint)
returns float
as
begin
	--declare @defaultValueId bigint
	--set @defaultValueId = 4002000022

	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.*
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select a.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'ligationDistribution'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'forwardScale'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)

	select @retval = cast(a.[value] as float)
	from t5
	cross apply openjson(t5.[value]) as a

	--print ('@retval = ' + isnull(cast(@retval as nvarchar(20)), '<null>'))
	--select * from t5

	return @retval
end
go

--declare @defaultValueId bigint
--set @defaultValueId = 4005000110

drop function if exists clm.getSugarBackward
go

create function clm.getSugarBackward(@defaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select i.[value] as [value]
		from openjson(@json) a
			cross apply openjson(a.[value]) as b
			cross apply openjson(b.[value]) as c
			cross apply openjson(c.[value]) as d
			cross apply openjson(d.[value]) as e
			cross apply openjson(e.[value]) as f
			cross apply openjson(f.[value]) as g
			cross apply openjson(g.[value]) as h
			cross apply openjson(h.[value]) as i
		where 
			a.[key] = 'rateParams'
			and c.[key] = 'Fields'
			and e.[key] = 'Fields'

			-- Can't yet get it better than this:
			and b.[key] = 4

			and g.[key] = 'backwardScale'
			and h.[key] = 'Fields'
	)

	select @retval = cast(t1.[value] as float)
	from t1

	return @retval
end
go
--declare @defaultValueId bigint
--set @defaultValueId = 4005000110

drop function if exists clm.getSugarForward
go

create function clm.getSugarForward(@defaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select i.[value] as [value]
		from openjson(@json) a
			cross apply openjson(a.[value]) as b
			cross apply openjson(b.[value]) as c
			cross apply openjson(c.[value]) as d
			cross apply openjson(d.[value]) as e
			cross apply openjson(e.[value]) as f
			cross apply openjson(f.[value]) as g
			cross apply openjson(g.[value]) as h
			cross apply openjson(h.[value]) as i
		where 
			a.[key] = 'rateParams'
			and c.[key] = 'Fields'
			and e.[key] = 'Fields'

			-- Can't yet get it better than this:
			and b.[key] = 4

			and g.[key] = 'forwardScale'
			and h.[key] = 'Fields'
	)

	select @retval = cast(t1.[value] as float)
	from t1

	return @retval
end
go
--declare @defaultValueId bigint
--set @defaultValueId = 4005000110

drop function if exists clm.getSugarScarcity
go

create function clm.getSugarScarcity(@defaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select m.[value] as [value]
		--select a.[key] as aKey, b.[key] as bKey, g.[key] as gKey, i.[value] as iValue, j.[value] as jValue, k.[key] as kKey, k.[value] as kValue, l.[key] as lKey, l.[value] as lValue, m.[value] as mValue
		from openjson(@json) a
			cross apply openjson(a.[value]) as b
			cross apply openjson(b.[value]) as c
			cross apply openjson(c.[value]) as d
			cross apply openjson(d.[value]) as e
			cross apply openjson(e.[value]) as f
			cross apply openjson(f.[value]) as g
			cross apply openjson(g.[value]) as h
			cross apply openjson(h.[value]) as i
			cross apply openjson(i.[value]) as j
			cross apply openjson(j.[value]) as k
			cross apply openjson(k.[value]) as l
			cross apply openjson(l.[value]) as m
		where
			a.[key] = 'rateParams'
			and c.[key] = 'Fields'
			and e.[key] = 'Fields'

			-- Can't yet get it better than this:
			--and b.[key] = 4

			and g.[key] = 'sugarSynthesisDistribution'
			and h.[key] = 'Fields'
			and k.[key] = 'threshold'
			and l.[key] = 'Fields'
	)

	--select * from t1
	select @retval = cast(t1.[value] as float)
	from t1

	return @retval
end
go
drop function if exists clm.getWasteRecyclingRate
go

create function clm.getWasteRecyclingRate(@defaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from clm.DefaultValue where defaultValueId = @defaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	select @retVal = cast(b.[value] as float)
	from t1
	cross apply openjson(t1.[value]) as a
	cross apply openjson(a.[value]) as b
	where b.[key] = 'wasteRecyclingRate'

	return @retVal
end
go
drop procedure if exists dbo.deleteRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure dbo.deleteRunQueue @runQueueId uniqueidentifier
as
begin
	declare @rowCount int
	set nocount on;

	delete from clm.RunQueue where runQueueId = @runQueueId
	delete from eeInf.RunQueue where runQueueId = @runQueueId
	delete from dbo.RunQueue where runQueueId = @runQueueId

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

drop procedure if exists dbo.tryResetRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure dbo.tryResetRunQueue @runQueueId uniqueidentifier
as
begin
	declare @rowCount int
	set nocount on;

	update dbo.RunQueue
	set
		runQueueStatusId = 0,
		errorMessage = null,
		workerNodeId = null,
		startedOn = null,
		modifiedOn = getdate()
	where runQueueId = @runQueueId and runQueueStatusId = 4

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

drop procedure if exists dbo.clm_updateTask
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure dbo.clm_updateTask
		@taskId uniqueidentifier,
		@remainingRepetitions int
as
begin
	declare @rowCount int
	set nocount on;

    update clm.Task
    set remainingRepetitions = @remainingRepetitions
    where taskId = @taskId

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

drop procedure if exists dbo.clm_upsertDefaultValue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure dbo.clm_upsertDefaultValue 
		@defaultValueId bigint,
		@defaultRateParams nvarchar(max),
		@description nvarchar(max)
as
begin
	declare @rowCount int
	set nocount on;

    merge clm.DefaultValue as target
    using (select @defaultValueId, @defaultRateParams, @description) as source (defaultValueId, defaultRateParams, description)
    on (target.defaultValueId = source.defaultValueId)
    when not matched then
        insert (defaultValueId, defaultRateParams, description)
        values (source.defaultValueId, source.defaultRateParams, source.description)
    when matched then
        update set defaultRateParams = source.defaultRateParams, description = source.description;

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

drop procedure if exists dbo.clm_upsertModelData
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure dbo.clm_upsertModelData 
		@modelDataId uniqueidentifier, 
		@taskId uniqueidentifier, 
		@seedValue int, 
		@modelDataParams nvarchar(max), 
		@modelBinaryData varbinary(max), 
		@createdOn datetime
as
begin
	declare @rowCount int
	set nocount on;

    merge clm.ModelData as target
    using (select @modelDataId, @taskId, @seedValue, @modelDataParams, @modelBinaryData, @createdOn)
    as source (modelDataId, taskId, seedValue, modelDataParams, modelBinaryData, createdOn)
    on (target.modelDataId = source.modelDataId)
    when not matched then
        insert (modelDataId, taskId, seedValue, modelDataParams, modelBinaryData, createdOn)
        values (source.modelDataId, source.taskId, source.seedValue, source.modelDataParams, source.modelBinaryData, source.createdOn)
    when matched then
        update set taskId = source.taskId, seedValue = source.seedValue, modelDataParams = source.modelDataParams, modelBinaryData = source.modelBinaryData, createdOn = source.createdOn;

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

drop procedure if exists dbo.eeInf_upsertModelData
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure dbo.eeInf_upsertModelData 
		@modelDataId uniqueidentifier, 
		@modelDataParams nvarchar(max), 
		@modelBinaryData varbinary(max), 
		@createdOn datetime
as
begin
	declare @rowCount int
	set nocount on;

    merge eeInf.ModelData as target
    using (select @modelDataId, @modelDataParams, @modelBinaryData, @createdOn)
    as source (modelDataId, modelDataParams, modelBinaryData, createdOn)
    on (target.modelDataId = source.modelDataId)
    when not matched then
        insert (modelDataId, modelDataParams, modelBinaryData, createdOn)
        values (source.modelDataId, source.modelDataParams, source.modelBinaryData, source.createdOn)
    when matched then
        update set modelDataParams = source.modelDataParams, modelBinaryData = source.modelBinaryData, createdOn = source.createdOn;

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

IF OBJECT_ID('[dbo].[DeliveryType]') IS NULL begin
	print 'Creating table [dbo].[DeliveryType] ...'

	CREATE TABLE [dbo].[DeliveryType](
		[deliveryTypeId] [int] NOT NULL,
		[deliveryTypeName] [nvarchar](50) NOT NULL,
	 CONSTRAINT [PK_DeliveryType] PRIMARY KEY CLUSTERED 
	(
		[deliveryTypeId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]
end else begin
	print 'Table [dbo].[DeliveryType] already exists ...'
end
go

IF OBJECT_ID('[dbo].[Message]') IS NULL begin
	print 'Creating table [dbo].[Message] ...'

	CREATE TABLE [dbo].[Message](
		[messageId] [uniqueidentifier] NOT NULL,
		[senderId] [uniqueidentifier] NOT NULL,
		[recipientId] [uniqueidentifier] NOT NULL,
		[messageOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[dataVersion] [int] NOT NULL,
		[deliveryTypeId] [int] NOT NULL,
		[messageData] [varbinary](max) NOT NULL,
		[createdOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_Message] PRIMARY KEY CLUSTERED 
	(
		[messageId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

	ALTER TABLE [dbo].[Message]  WITH CHECK ADD  CONSTRAINT [FK_Message_DeliveryType] FOREIGN KEY([deliveryTypeId])
	REFERENCES [dbo].[DeliveryType] ([deliveryTypeId])

	ALTER TABLE [dbo].[Message] CHECK CONSTRAINT [FK_Message_DeliveryType]
end else begin
	print 'Table [dbo].[Message] already exists ...'
end
go

drop procedure if exists deleteExpiredMessages
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure deleteExpiredMessages (@dataVersion int, @createdOn datetime)
as
begin
	declare @rowCount int
	set nocount on;

    delete from dbo.Message
    where
        deliveryTypeId = 1
        and dataVersion = @dataVersion
        and createdOn < @createdOn

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

drop procedure if exists deleteMessage
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure deleteMessage @messageId uniqueidentifier
as
begin
	declare @rowCount int
	set nocount on;

	delete from dbo.Message where messageId = @messageId

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

drop procedure if exists saveMessage
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure saveMessage (
					@messageId uniqueidentifier,
					@senderId uniqueidentifier,
					@recipientId uniqueidentifier,
					@dataVersion int,
					@deliveryTypeId int,
					@messageData varbinary(max))
as
begin
	declare @rowCount int
	set nocount on;

	insert into Message (messageId, senderId, recipientId, dataVersion, deliveryTypeId, messageData, createdOn)
	select @messageId, @senderId, @recipientId, @dataVersion, @deliveryTypeId, @messageData, getdate()
	where not exists (select 1 from Message where messageId = @messageId)

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

;with 
	valTbl as
	(
		select * 
		from 
		( values
			  (1, 'ClmModel')
			, (2, 'EeInfModel')

		) as a (modelTypeId, modelTypeName)
	)
insert into ModelType
select valTbl.*
from valTbl
left outer join ModelType on valTbl.modelTypeId = ModelType.modelTypeId
where ModelType.modelTypeId is null
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
			  (0, 'Active')
			, (1, 'Inactive')

		) as a (taskStatusId, taskStatusName)
	)
insert into clm.TaskStatus
select valTbl.*
from valTbl
left outer join clm.TaskStatus on valTbl.taskStatusId = clm.TaskStatus.TaskStatusId
where clm.TaskStatus.TaskStatusId is null
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


