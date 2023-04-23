IF OBJECT_ID('[dbo].[ClmResultData]') IS NULL begin
	print 'Creating table [dbo].[ClmResultData] ...'

	CREATE TABLE [dbo].[ClmResultData](
		[clmResultDataId] [uniqueidentifier] NOT NULL, -- resultId is always runQueueId
		[clmResultDataOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[clmModelDataId] [uniqueidentifier] NOT NULL,
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
	 CONSTRAINT [PK_ClmResultData] PRIMARY KEY CLUSTERED 
	(
		[clmResultDataId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [dbo].[ClmResultData] ADD  DEFAULT ((0)) FOR [useAbundant]
	ALTER TABLE [dbo].[ClmResultData] ADD  DEFAULT ((0)) FOR [maxEe]
	ALTER TABLE [dbo].[ClmResultData] ADD  DEFAULT ((0)) FOR [maxAverageEe]
	ALTER TABLE [dbo].[ClmResultData] ADD  DEFAULT ((0)) FOR [maxWeightedAverageAbsEe]
	ALTER TABLE [dbo].[ClmResultData] ADD  DEFAULT ((0)) FOR [maxLastEe]
	ALTER TABLE [dbo].[ClmResultData] ADD  DEFAULT (getdate()) FOR [createdOn]
	ALTER TABLE [dbo].[ClmResultData] ADD  DEFAULT (getdate()) FOR [modifiedOn]

	--ALTER TABLE [dbo].[ClmResultData]  WITH CHECK ADD  CONSTRAINT [FK_ClmResultData_WorkerNode] FOREIGN KEY([workerNodeId])
	--REFERENCES [dbo].[WorkerNode] ([workerNodeId])
	--ALTER TABLE [dbo].[ClmResultData] CHECK CONSTRAINT [FK_ClmResultData_WorkerNode]

	ALTER TABLE [dbo].[ClmResultData]  WITH CHECK ADD  CONSTRAINT [FK_ClmResultlData_ClmModelData] FOREIGN KEY([clmModelDataId])
	REFERENCES [dbo].[ClmModelData] ([clmModelDataId])
	ALTER TABLE [dbo].[ClmResultData] CHECK CONSTRAINT [FK_ClmResultlData_ClmModelData]

	ALTER TABLE [dbo].[ClmResultData]  WITH CHECK ADD  CONSTRAINT [FK_ClmResultData_RunQueue] FOREIGN KEY([clmResultDataId])
	REFERENCES [dbo].[RunQueue] ([runQueueId])
	ALTER TABLE [dbo].[ClmResultData] CHECK CONSTRAINT [FK_ClmResultData_RunQueue]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_ResultData] ON [dbo].[ClmResultData]
	(
		[clmResultDataOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

end else begin
	print 'Table [dbo].[ClmResultData] already exists ...'
end
go




