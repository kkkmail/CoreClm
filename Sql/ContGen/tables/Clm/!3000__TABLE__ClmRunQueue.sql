IF OBJECT_ID('[dbo].[ClmRunQueue]') IS NULL begin
	print 'Creating table [dbo].[ClmRunQueue] ...'

	CREATE TABLE [dbo].[ClmRunQueue](
		[runQueueId] [uniqueidentifier] NOT NULL,
		[clmModelDataId] [uniqueidentifier] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL,
		[maxEe] [float] NOT NULL,
		[maxAverageEe] [float] NOT NULL,
		[maxWeightedAverageAbsEe] [float] NOT NULL,
		[maxLastEe] [float] NOT NULL,
	 CONSTRAINT [PK_ClmRunQueue] PRIMARY KEY CLUSTERED 
	(
		[runQueueId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [dbo].[ClmRunQueue] ADD  DEFAULT ((0)) FOR [useAbundant]
	ALTER TABLE [dbo].[ClmRunQueue] ADD  DEFAULT ((0)) FOR [maxEe]
	ALTER TABLE [dbo].[ClmRunQueue] ADD  DEFAULT ((0)) FOR [maxAverageEe]
	ALTER TABLE [dbo].[ClmRunQueue] ADD  DEFAULT ((0)) FOR [maxWeightedAverageAbsEe]
	ALTER TABLE [dbo].[ClmRunQueue] ADD  DEFAULT ((0)) FOR [maxLastEe]

	ALTER TABLE [dbo].[ClmRunQueue]  WITH CHECK ADD  CONSTRAINT [FK_ClmRunQueue_RunQueue] FOREIGN KEY([runQueueId])
	REFERENCES [dbo].[RunQueue] ([runQueueId])
	ALTER TABLE [dbo].[ClmRunQueue] CHECK CONSTRAINT [FK_ClmRunQueue_RunQueue]

	ALTER TABLE [dbo].[ClmRunQueue]  WITH CHECK ADD  CONSTRAINT [FK_ClmRunQueue_ClmModelData] FOREIGN KEY([clmModelDataId])
	REFERENCES [dbo].[ClmModelData] ([clmModelDataId])
	ALTER TABLE [dbo].[ClmRunQueue] CHECK CONSTRAINT [FK_ClmRunQueue_ClmModelData]

end else begin
	print 'Table [dbo].[ClmRunQueue] already exists ...'
end
go



