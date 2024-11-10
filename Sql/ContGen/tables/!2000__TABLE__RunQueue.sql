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



