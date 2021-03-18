IF OBJECT_ID('[dbo].[ResultData]') IS NULL begin
	print 'Creating table [dbo].[ResultData] ...'

	CREATE TABLE [dbo].[ResultData](
		[resultDataId] [uniqueidentifier] NOT NULL,
		[resultDataOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[workerNodeId] [uniqueidentifier] NOT NULL,
		[modelDataId] [uniqueidentifier] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL,
		[maxEe] [float] NOT NULL,
		[maxAverageEe] [float] NOT NULL,
		[maxWeightedAverageAbsEe] [float] NOT NULL,
		[maxLastEe] [float] NOT NULL,
		[createdOn] [datetime] NOT NULL,
		[modifiedOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_ResultData] PRIMARY KEY CLUSTERED 
	(
		[resultDataId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [dbo].[ResultData] ADD  DEFAULT ((0)) FOR [useAbundant]
	ALTER TABLE [dbo].[ResultData] ADD  DEFAULT ((0)) FOR [maxEe]
	ALTER TABLE [dbo].[ResultData] ADD  DEFAULT ((0)) FOR [maxAverageEe]
	ALTER TABLE [dbo].[ResultData] ADD  DEFAULT ((0)) FOR [maxWeightedAverageAbsEe]
	ALTER TABLE [dbo].[ResultData] ADD  DEFAULT ((0)) FOR [maxLastEe]
	ALTER TABLE [dbo].[ResultData] ADD  DEFAULT (getdate()) FOR [createdOn]
	ALTER TABLE [dbo].[ResultData] ADD  DEFAULT (getdate()) FOR [modifiedOn]

	ALTER TABLE [dbo].[ResultData]  WITH CHECK ADD  CONSTRAINT [FK_ResultData_WorkerNode] FOREIGN KEY([workerNodeId])
	REFERENCES [dbo].[WorkerNode] ([workerNodeId])

	ALTER TABLE [dbo].[ResultData] CHECK CONSTRAINT [FK_ResultData_WorkerNode]

	ALTER TABLE [dbo].[ResultData]  WITH CHECK ADD  CONSTRAINT [FK_ResultlData_ModelData] FOREIGN KEY([modelDataId])
	REFERENCES [dbo].[ModelData] ([modelDataId])

	ALTER TABLE [dbo].[ResultData] CHECK CONSTRAINT [FK_ResultlData_ModelData]

	ALTER TABLE [dbo].[ResultData]  WITH CHECK ADD  CONSTRAINT [FK_ResultData_RunQueue] FOREIGN KEY([resultDataId])
	REFERENCES [dbo].[RunQueue] ([runQueueId])

	ALTER TABLE [dbo].[ResultData] CHECK CONSTRAINT [FK_ResultData_RunQueue]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_ResultData] ON [dbo].[ResultData]
	(
		[resultDataOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

end else begin
	print 'Table [dbo].[ResultData] already exists ...'
end
go




