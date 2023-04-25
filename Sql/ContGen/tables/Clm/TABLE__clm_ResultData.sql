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
	REFERENCES [clm].[ModelData] ([clmModelDataId])
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




