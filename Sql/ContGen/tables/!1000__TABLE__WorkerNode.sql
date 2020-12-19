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

