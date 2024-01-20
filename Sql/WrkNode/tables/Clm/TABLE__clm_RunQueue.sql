IF OBJECT_ID('[clm].[RunQueue]') IS NULL begin
	print 'Creating table [clm].[RunQueue] ...'

	CREATE TABLE [clm].[RunQueue](
		[runQueueId] [uniqueidentifier] NOT NULL,
		[maxEe] [float] NOT NULL,
		[maxAverageEe] [float] NOT NULL,
		[maxWeightedAverageAbsEe] [float] NOT NULL,
		[maxLastEe] [float] NOT NULL,
	 CONSTRAINT [PK_clm_RunQueue] PRIMARY KEY CLUSTERED 
	(
		[runQueueId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [clm].[RunQueue] ADD DEFAULT ((0)) FOR [maxEe]
	ALTER TABLE [clm].[RunQueue] ADD DEFAULT ((0)) FOR [maxAverageEe]
	ALTER TABLE [clm].[RunQueue] ADD DEFAULT ((0)) FOR [maxWeightedAverageAbsEe]
	ALTER TABLE [clm].[RunQueue] ADD DEFAULT ((0)) FOR [maxLastEe]

	ALTER TABLE [clm].[RunQueue]  WITH CHECK ADD  CONSTRAINT [FK_clm_RunQueue_RunQueue] FOREIGN KEY([runQueueId])
	REFERENCES [dbo].[RunQueue] ([runQueueId])
	ALTER TABLE [clm].[RunQueue] CHECK CONSTRAINT [FK_clm_RunQueue_RunQueue]

end else begin
	print 'Table [clm].[RunQueue] already exists ...'
end
go



